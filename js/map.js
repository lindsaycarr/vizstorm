// Width and height
var chart_width     =   1000;
var chart_height    =   600;

// Projection
var projection = d3.geoAlbers()
    .scale([1800])
    // default is .rotate([96,0]) to center on US
    .center([96-91.34397, 32.25196]) // adjust longitude relative to rotation
    .translate([chart_width / 2, chart_height / 2]);
var path = d3.geoPath()
    .projection(projection);

// Create SVG
var svg = d3.select("body")
    .append("svg")
    .attr("width", chart_width)
    .attr("height", chart_height);

var map = svg.append( 'g' )
    .attr( 'id', 'map' );
      
// Define the div for the tooltip
var div = d3.select("body")
    .append("div")
    .attr("class", "tooltip");
		
// setup var to map precip data to cells
var precip = d3.map();

// precip color scale
var color = d3.scaleThreshold()
    .range(d3.schemeBlues[9]);

// Add map features, need to queue to load more than one json
d3.queue()
  .defer(d3.json, "data/state_map.geojson")
  .defer(d3.json, "data/county_map.geojson")
  .defer(d3.json, "data/precip_cells.geojson")
  .defer(d3.json, "data/precip_cell_data.json")
  .defer(d3.json, "data/precip_data_range.json")
  .await(createMap);

function createMap() {
  
  // arguments[0] is the error
	var error = arguments[0];
	
	// the rest of the indices of arguments are all the other arguments passed in
	// so in this case, all of the results from q.defers
	var state_data = arguments[1];
	var county_data = arguments[2];
	var precip_cells = arguments[3];
	var precip_data = arguments[4];
	var precip_range = arguments[5];
	
	// update color scale using data
  color.domain(precip_range.breaks);
  
  // add states
  map.append("g").attr('id', 'statepolygons')
        .selectAll( 'path' )
        .data(state_data.features)
        .enter()
        .append('path')
        .attr('d', path);
  
  // add counties
  map.append("g").attr('id', 'countypolygons')
        .selectAll( 'path' )
        .data(county_data.features)
        .enter()
        .append('path')
        .attr('d', path)
        .on("mouseover", mouseover)
        .on("mouseout", mouseout);
    
  // add state outline on top of counties
  map.append("g").attr('id', 'stateoutline')
        .selectAll( 'path' )
        .data(state_data.features)
        .enter()
        .append('path')
        .attr('d', path); // pointer events passed to county layer
  
  // animate over time steps
  // start by initializing the first timestep
  var all_timesteps = Object.keys(precip_data);
  var timestep = 0;
  var precip_ts = precip_data[all_timesteps[timestep]];
  
  // add precip cells on top of everything else
  map.append("g").attr('id', 'precipcells')
        .selectAll( 'path' )
        .data(precip_cells.features)
        .enter()
        .append('path')
        .attr('d', path)
        .attr('fill', function(d) {
          var precip = extractPrecipVal(precip_ts, d.properties.ID);
          return getPrecipColor(precip);
        });
  
  map.append("text")
        .attr("class", "timestamp")
        .attr("x", (chart_width / 2))             
        .attr("y", (0.15*chart_height))
        .text(all_timesteps[timestep]);
        
  var interval = setInterval(function() {
    timestep++;
    if (timestep >= all_timesteps.length) { 
      clearInterval(interval); 
    } else {
      // update precip data used
      precip_ts = precip_data[all_timesteps[timestep]];
      changeColor(precip_ts);
      updateTitle(all_timesteps[timestep]);
    }
  }, 0);
    
}

function mouseover(d) {
  var x_val = d3.event.pageX; 
	var y_val = d3.event.pageY;
	var y_buffer = y_val*0.05; // move text above mouse by 5% of yval
	
	d3.select(".tooltip")
		.style("display", "block")
		.style("left", x_val+"px")
		.style("top", (y_val-y_buffer)+"px")
		.text(formatCountyName(d.properties.ID));
  
  d3.select(this)
    .classed('hover', true); 
}

function mouseout(d) {
  d3.selectAll(".tooltip")
		.style("display", "none");
		
  d3.select(this)
    .classed('hover', false);
}

function formatCountyName(nm) {
  return nm.split(",").reverse().join(", ");
}

function extractPrecipVal(precip_ts, cell_id) {
  // there's got to be a cleaner way to do this ...
  var precip_cell = precip_ts.filter(function(i) {
    return i.cell == cell_id;
  });
  var precip_val = precip_cell[0].precip;
  return precip_val;
}

function getPrecipColor(precip_val) {
  //need if statement, adding "transparent" to array did not work
  if(precip_val > 0) { 
    return color(precip_val); 
  } else {
    return "transparent";
  } 
}

function changeColor(precip_ts) {
  
  map.selectAll("#precipcells path")
      .transition()
      .duration(500)
      .attr('fill', function(d) { 
          var precip = extractPrecipVal(precip_ts, d.properties.ID);
          return getPrecipColor(precip); 
      }); 
}

function updateTitle(new_date) {
  
  map.selectAll(".timestamp")
      .transition()
      .duration(500)
      .text(new_date); 
}


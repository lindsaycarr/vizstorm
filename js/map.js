// Width and height
var chart_width     =   1000;
var chart_height    =   600;

// Projection
var projection = d3.geoMercator() //changing to geoAlbers makes the map disappear
    .scale(1500)
    .center([-91.34397, 32.25196])
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
    .attr("class", "tooltip")
		.style("pointer-events", "none")
		.style("background", "rgba(255,255,255,0.8)") //white, slightly transparent
		.style("border", "3px")
		.style("border-radius", "8px")
		.style("font-family", "sans-serif");
		
// setup var to map precip data to cells
var precip = d3.map();

// precip color scale
var color_vals = d3.schemeBlues[9];
color_vals.unshift("transparent"); //add to front of color array
var color = d3.scaleThreshold()
    .domain(d3.range(0, 10)) // need better way to get range of data, should do inside queue
    .range(color_vals);

// Add map features, need to queue to load more than one json
d3.queue()
  .defer(d3.json, "../cache/state_map.geojson")
  .defer(d3.json, "../cache/county_map.geojson")
  .defer(d3.json, "../cache/precip_cells.geojson")
  .defer(d3.csv, "../cache/precip_cell_data.csv", function(d) { 
    precip.set(d.cell, +d.precip); 
  })
  .await(createMap);

function createMap() {
  
  // arguments[0] is the error
	var error = arguments[0];
	
	// the rest of the indices of arguments are all the other arguments passed in
	// so in this case, all of the results from q.defers
	var state_data = arguments[1];
	var county_data = arguments[2];
	var precip_cells = arguments[3];
  
  // add states
  map.append("g").attr('id', 'statepolygons')
        .selectAll( 'path' )
        .data(state_data.features)
        .enter()
        .append('path')
        .attr('d', path)
        .attr('fill', "#9a9a9a")
        .attr('stroke', 'none');
  
  // add counties
  map.append("g").attr('id', 'countypolygons')
        .selectAll( 'path' )
        .data(county_data.features)
        .enter()
        .append('path')
        .attr('d', path)
        .attr('fill', "#efefef")
        .attr('stroke', '#c6c6c6')
        .attr('stroke-width', 0.5)
        .on("mouseover", mouseover)
        .on("mouseout", mouseout);
    
  // add state outline on top of counties
  map.append("g").attr('id', 'stateoutline')
        .selectAll( 'path' )
        .data(state_data.features)
        .enter()
        .append('path')
        .attr('d', path)
        .attr("pointer-events", "none") // pointer events passed to county layer
        .attr('fill', "transparent")
        .attr('stroke', '#5f5f5f')
        .attr('stroke-width', 0.5);
  
  // add precip cells on top of everything else
  map.append("g").attr('id', 'precipcells')
        .selectAll( 'path' )
        .data(precip_cells.features)
        .enter()
        .append('path')
        .attr('d', path)
        .attr("pointer-events", "none") // pointer events passed to county layer
        .attr('fill', function(d) { 
          d.precip = precip.get(d.properties.ID); //use "get" to grab precip from the match ID
          return color(d.precip); 
        })
        .attr('stroke', 'none')
        .attr("opacity", "0.7");

}

function mouseover(d) {
  var x_val = d3.event.pageX; 
	var y_val = d3.event.pageY;
	var y_buffer = y_val*0.05; // move text above mouse by 5% of yval
	
	d3.select(".tooltip")
		.style("display", "block")
		.style("position", "absolute")
		.style("left", x_val+"px")
		.style("top", (y_val-y_buffer)+"px")
		.style("text-anchor", "end")
		.style("text-transform", "capitalize")
		.text(formatCountyName(d.properties.ID));
  
  d3.select(this).style('fill', 'orange'); 
}

function mouseout(d) {
  d3.selectAll(".tooltip")
		.style("display", "none");
		
  d3.select(this).style('fill', "#efefef");
}

function formatCountyName(nm) {
  return nm.split(",").reverse().join(", ");
}



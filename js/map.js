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

// Add map features, need to queue to load more than one json
d3.queue()
  .defer(d3.json, "../cache/state_map.geojson")
  .defer(d3.json, "../cache/county_map.geojson")
  .await(createMap);

function createMap() {
  
  // arguments[0] is the error
	var error = arguments[0];
	
	// the rest of the indices of arguments are all the other arguments passed in
	// so in this case, all of the results from q.defers
	var state_data = arguments[1];
	var county_data = arguments[2];
        
  // add counties
  map.append("g").attr('id', 'countymap')
        .selectAll( 'path' )
        .data(county_data.features)
        .enter()
        .append('path')
        .attr('d', path)
        .attr('fill', "cornflowerblue")
        .attr('stroke', 'white')
        .attr('stroke-width', 0.25)
        .on("mouseover", mouseover)
        .on("mouseout", mouseout);
    
  // add states on top of counties
  map.append("g").attr('id', 'statemap')
        .selectAll( 'path' )
        .data(state_data.features)
        .enter()
        .append('path')
        .attr('d', path)
        .attr("pointer-events", "none") // pointer events passed to county layer
        .attr('fill', "transparent")
        .attr('stroke', 'black')
        .attr('stroke-width', 2);
}

function mouseover(d) {
  d3.select(this).style('fill', 'orange'); 
}

function mouseout(d) {
  d3.select(this).style('fill', "cornflowerblue");
}

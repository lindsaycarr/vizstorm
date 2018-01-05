// Width and height
var chart_width     =   800;
var chart_height    =   600;

// Projection
var projection = d3.geoMercator() //changing to geoAlbers makes the map disappear
    .scale(1500)
    .center([-91.34397, 32.25196])
    //.translate([0,0]);
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

// Data
d3.json('../cache/state_map.geojson', function(us_data){
    
    // us_data.features.forEach(function(d) {
    //   d.geometry.coordinates[0][0] = parseFloat(d.geometry.coordinates[0][0]);
    // });
    
    console.log(us_data);
    //console.log(us_data.features[0].geometry.coordinates[0][0]);

    map.selectAll( 'path' )
        .data(us_data.features)
        .enter()
        .append('path')
        .attr('d', path)
        .attr('fill', "cornflowerblue")
        .attr('stroke', '#fff')
        .attr('stroke-width', 0.5)
        .on("mouseover", mouseover)
        .on("mouseout", mouseout);

});

function mouseover(d){
  // Highlight hovered province
  d3.select(this).style('fill', 'orange');
}

function mouseout(d){
  // Reset province color
  d3.select(this).style('fill', "cornflowerblue");
}

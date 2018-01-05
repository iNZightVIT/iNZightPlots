// For hexagonal bin plots:

// hide table button:
d3.select('#viewTable').attr('class', 'hidden');

// set up tooltips:
var Grob = getGrob('hexbin');
var panel = document.getElementById(Grob);
var count = data.length;
var tooltip = d3.select('body').append('div')
              .attr('class', 'tooltip')
              .attr('id', 'tooltip')
              .style('width', '120')
              .style('height', '45');

d3.select(panel).selectAll('polygon')
    .data(data)
    .attr('class', 'hexbin')
    .on('mouseover', function(d){tooltip.style('visibility', 'visible')
                                        .style("left", d3.event.pageX - 40 + "px")
                                        .style("top", d3.event.pageY - 55 + "px")
                                        .html("N = <span>" + d.counts + ", " + d.pct +
                                              "% </span>" + "<br> Centered at: <br> <span>" +
                                              d.xcm + ", " + d.ycm + "</span>");})
    .on('mouseout', function(){tooltip.style("visibility", "hidden");})
    .on('click', function(d, i) {
        var selected = this;
        d3.select(panel).selectAll('.hexbin')
          .attr("class", function() {
            return (this === selected ? "hexbin selected" : "hexbin none");
        })
      d3.select("#tt")
        .style('visibility', 'hidden');
    });

//reset button:
reset = function() {
      d3.select(panel).selectAll('polygon')
      .attr("class", "hexbin");
      // hide brush
      d3.selectAll(".selection")
        .style("display", "none");
}

// create another tooltip for selection box:
var tt = d3.select('#control').append('div')
             .attr('class', 'tooltip')
             .attr('id', 'tt')
             .style('width', '150')
             .style('padding', '5px')
             .style('margin-top', '5px')
             .style('height', '35');

//create brush:
var svg = d3.select('svg');

var brush = d3.brush()
              .on("start", brushstart)
              .on("brush", brushmove)
              .on("end", brushend);

var pp = panel.parentNode;
d3.select(pp)
  .insert("g", "g:nth-child(4)") //"g" ":first-child"
  .attr("class", "brush")
  .call(brush);

// make handles invisible:
d3.selectAll('.handle')
  .style('opacity', 0);

function brushmove() {

  var s = d3.event.selection;
  var x1 = s[0][0];
  var x2 = s[1][0];
  var y1 = s[0][1];
  var y2 = s[1][1];

  // information to extract:
  var groupN = [];
  var n = chart.n;

  for (i =1; i <= count; i++) {
      var hexbin = document.getElementById(Grob + '.' + i);
      //find the x value at the center of the hexagon (midpoint):
      var coords = hexbin.getAttribute('points');
      var topEdge = coords.split(' ')[5];
      var bottomEdge = coords.split(' ')[2];
      var x = Number(topEdge.split(",")[0]); //midpoint x
      var y = (Number(topEdge.split(",")[1]) + Number(bottomEdge.split(",")[1]))/2; //midpoint y

      if (hexbin.getAttribute('visibility') == 'hidden') {
         // those that are hidden, remain hidden
         hexbin.classList.add('hidden');
       } else {
        //points that lie within the  boundary box drawn:
          if ((x1 <= x && x <= x2) && (y1 <= y && y <= y2)) {
            hexbin.setAttribute('class', ' hexbin selected');
             // store no. of counts from each hexbin that's selected
             groupN.push(data[i-1].counts);
           } else {
             hexbin.setAttribute('class', 'hexbin none');
           }
        }
     }

     //summation of array + get proportion:
     var sum = groupN.reduce(function(a, b) { return a + b; }, 0);
     var nProp = (sum/n*100).toFixed(2) + "%";
     var nbins = document.getElementsByClassName('selected').length;

    //update tooltip
    d3.select('#tt')
      .style('visibility', 'visible')
      .html("Frequency: <span>" + sum + ", " + nProp +
             "</span>" + "<br> bins: <span>" + nbins + "</span>");
};

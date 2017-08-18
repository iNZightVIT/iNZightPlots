// For hexagonal bin plots:

d3.select('#viewTable').attr('class', 'hidden');

// hexbin tooltip using D3:
var Grob = getGrob('hexbin');
var panel = document.getElementById(Grob);
var count = tab.length;
var n = 11000;
var tooltip = d3.select('body').append('div')
              .attr('class', 'tooltip')
              .attr('id', 'tooltip')
              .style('width', '120')
              .style('height', '45');

d3.select(panel).selectAll('polygon')
    .data(tab)
    .attr('class', 'hexbin')
    .on('mouseover', function(d){tooltip.style('visibility', 'visible')
                                              .style("left", d3.event.pageX - 40 + "px")
                                              .style("top", d3.event.pageY - 55 + "px")
                                              .html("N = <span>" + d.counts + ", " + d.pct +
                                              "% </span>" + "<br> Centered at: <br> <span>" +
                                              d.xcm + ", " + d.ycm + "</span>");})
    .on('mouseout', function(){tooltip.style("visibility", "hidden");})
    .on('click', function(d, i) {
      for(j = 1; j <= count; j ++) { //could refactor this?
        var hexbin = document.getElementById(Grob + '.' + j);
        if ( (j-1) == i) {
          hexbin.setAttribute('class', 'hexbin selected');
        } else {
          hexbin.setAttribute('class', 'hexbin none');
        }
      }
      tt2.style('visibility', 'hidden');
    });

//reset button:
d3.select('#reset')
  .on('click', function() {
    d3.select(panel).selectAll('polygon')
    .attr("class", "hexbin");
    d3.select('#selectRect')
      .attr('points', '0,0')
      .attr("class", "selectRect");
      d3.select('#selection')
      .style('visibility', 'hidden');
  });

//User selection drag box:

 // create another  tooltip for selection box:
var tt2 = d3.select('body').append('div')
             .attr('class', 'tooltip')
             .attr('id', 'selection')
             .style('width', '150')
             .style('height', '35');

//create invisible selection box that is enabled when dragging occurs:
d3.select(panel).append('polygon')
    .attr('id', 'selectRect')
    .attr('class', 'selectRect');

  var svg = document.getElementsByTagName('svg')[0];
  var evt = window.event;
  var zoomBox = {};

   svg.setAttribute('onmouseup', 'MouseUp(evt)');
   svg.setAttribute('onmousemove', 'MouseDrag(evt)');
   svg.setAttribute('onmousedown', 'MouseDown(evt)'); //defined below.

   MouseDrag = function(evt) {
       if(zoomBox["isDrawing"]) {
         var pt = convertCoord(svg, evt)
         svg.style.cursor = "crosshair";
           zoomBox["endX"] = pt.x;
           zoomBox["endY"] = pt.y;

           //Because the y-axis is inverted in the plot - need to invert the scale
            tVal = document.getElementsByTagName('g')[0].getAttribute('transform').substring(13, 16);
           var selectRect = document.getElementById('selectRect');

            // for rectangles with positive height, positive width
           if(zoomBox["startX"] < zoomBox["endX"]) {
           var x1 = zoomBox["startX"];
           var x2 = zoomBox["endX"];
         } else {
           var x1 = zoomBox["endX"];
           var x2 = zoomBox["startX"];
         }

         // for rectangles with opposite directions ('negative' widths, heights)
         if (zoomBox["startY"] < zoomBox["endY"]) {
           var y1 = tVal - zoomBox["startY"] - (zoomBox["endY"]-zoomBox["startY"]);
           var y2 = y1 + (zoomBox["endY"]-zoomBox["startY"]);
         } else {
           var y1 = tVal - zoomBox["endY"] - (zoomBox["startY"]-zoomBox["endY"]);
           var y2 = y1 + (zoomBox["startY"]-zoomBox["endY"]);
         }

           selectRect.setAttribute('points', x1 + ',' + y1 + " " + x1 + ',' + y2 + ' '
                                             + x2 + ',' + y2 + ' ' + x2 + ',' + y1);

          // information to extract:
          var groupN = [];

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
             if((x1 <= x && x <= x2) && (y1 <= y && y <= y2)) {
               hexbin.setAttribute('class', ' hexbin selected');
              // store no. of counts from each hexbin that's selected
               groupN.push(tab[i-1].counts);

              } else {
                hexbin.setAttribute('class', 'hexbin none');
              }
            }
           }

           //summation of array:
           var sum = groupN.reduce(function(a, b) { return a + b; }, 0);

           // report a proportion:
           var nProp = (sum/n*100).toFixed(2) + "%";

             //information to extract:
           var nbins = document.getElementsByClassName('selected').length;

           // create another  tooltip for selection box:
                       tt2.style("left", ((x1+x2)/2) + "px"); //positioning!
                       tt2.style("top", tVal - (y1 - 30) + "px");
                       tt2.style('visibility', 'visible');
                       tt2.html("Frequency: <span>" + sum + ", " + nProp +
                       "</span>" + "<br> bins: <span>" +
                       nbins + "</span>");
       }
   };

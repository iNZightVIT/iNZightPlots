//histograms!

var table = document.getElementById('table');
    nrow = table.rows.length, //no. of rows in table
    ncol = document.getElementsByTagName('th').length, //no. of columns in table
    td = document.getElementsByTagName('td'),
    tr = document.getElementsByTagName('tr'),
    totalRow = tr[nrow-1],
    cellNo = td.length;

for (i = 1; i <= cellNo; i++) {
  td[i-1].setAttribute('id', 'td' + i);
};

for (i = 1; i < nrow; i++) {
  tr[i].setAttribute('id', 'tr' + i);
};

totalRow.setAttribute('class', 'tc');

//drive the viewTable button:
  var viewTable = document.getElementById('viewTable');
  t = true;
showTable = function() {
  if(t) {
    viewTable.innerHTML = "Hide Table";
    table.classList.remove('hidden');
    t = false;
  } else {
    viewTable.innerHTML = "View Table";
    table.classList.add('hidden');
    t = true
  }
};

var svg = document.getElementsByTagName('svg')[0];

var count = tab.length;

//get Grob object where hist bars lie:
var Grob = getGrob('hist');

var tooltip = d3.select('body').append('div')
              .attr('class', 'tooltip')
              .attr('id', 'tooltip')
              .style('width', '200')
              .style('height', '50');

var panel = document.getElementById(Grob);
d3.select(panel).selectAll('polygon')
    .data(tab)
    .attr('class', 'histBar')
    .on('mouseover', function(d){tooltip.style('visibility', 'visible')
                                              .style("left", d3.event.pageX - 40 + "px")
                                              .style("top", d3.event.pageY - 55 + "px")
                                              .html("Class range: <br> <span>" + d.lower + " - " + d.upper +
                                              " </span>" + "<br> N = <span>" +
                                              d.counts + ", " + d.pct + "% </span>");})
    .on('mouseout', function(){tooltip.style("visibility", "hidden");})
    .on('click', function(d, i) {
        for (j = 1; j <= count; j ++) {
          var bar = document.getElementById(Grob + '.' + j);
          var dataRow = document.getElementById('tr' + j);
          var totalRow = document.getElementsByClassName('tc')[0];
          totalRow.classList.add('hidden');

          if ((i+1) == j) {
            bar.setAttribute('class', 'histBar selected');
           var l = bar.getAttribute('fill');
           var lp = l.substring(l.lastIndexOf("("), l.lastIndexOf(")"));

           returnRowSelection(lp, dataRow);

          } else {
            bar.setAttribute('class', 'histBar none');
            omitRowSelection(dataRow);

          }
        hideBox();
        tt2.style('visibility', 'hidden');
      }
    });

/* -------------------------------------------------------------
                  Box plot properties and interactions:
---------------------------------------------------------------- */

//identify lines and box plot:
var lastLine = getMinMaxLinesId();
getBoxes("hist");

//set labels
boxLabelSet(0, 1, 0,'LQ');
boxLabelSet(1, 2, 2, 'UQ');
boxLabelSet(1, 0, 1, 'Median');
boxLabelSet(1, 0, 3, 'Min');
boxLabelSet(2, 1, 4, 'Max');

//group boxplot together as a single object
var box = document.getElementsByClassName('box');
var boxData = document.getElementsByClassName('boxData');
var totalRow = document.getElementById('totalRow');

//setting interactions and colors for box plot:
for (i = 0; i < box.length; i++) {
  box[i].addEventListener('mouseover', fillBox, false);
  box[i].addEventListener('mouseout', normalBox, false);
  box[i].addEventListener('click', showBox, false);
}

 // facilitate selection box for users:

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

var evt = window.event; // required for FF to work.
var zoomBox = {};

// assign mouse events:
svg.setAttribute('onmouseup', 'MouseUp(evt)');
svg.setAttribute('onmousemove', 'MouseDrag(evt)');
svg.setAttribute('onmousedown', 'MouseDown(evt)'); // defined below.

MouseDrag = function(evt) {
    if(zoomBox["isDrawing"]) {
        var pt = convertCoord(svg, evt);
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
        var intRange = [];

        for (i =1; i <= count; i++) {
        var bar = document.getElementById(Grob + '.' + i);
        //var gLabel = document.getElementById('gLabel' + i);
        var dataRow = document.getElementById('tr' + i);

        //obtain end points of the bar:
        var coords = bar.getAttribute('points').split(" ");
        var bottomEdge = coords[0].split(',');
        var topEdge =  coords[3].split(',');
        var bx = bottomEdge[0];
        var by = bottomEdge[1];
        var tx = topEdge[0];
        var ty = topEdge[1];

          if (bar.getAttribute('visibility') == 'hidden') {
            // those that are hidden, remain hidden
              bar.classList.add('hidden');
            } else {
              //points that lie within the  boundary box drawn:
          if(((x1 <= bx && bx <= x2) && (x1 <= tx && tx <= x2)) && ((y1 <= by && by <= y2) && (y1 <= ty && ty <= y2))) {
            bar.setAttribute('class', ' histBar selected');
            l = bar.getAttribute('fill');
            lp = l.substring(l.lastIndexOf("("), l.lastIndexOf(")"));

            returnRowSelection(lp, dataRow);

           // store frequency from each hexbin that's selected
            groupN.push(tab[i].counts);

            intRange.push(tab[i].lower, tab[i].upper);

           } else {
             bar.setAttribute('class', 'histBar none');
             omitRowSelection(dataRow);

           }
         }

         //summation of array:
         var sum = groupN.reduce(function(a, b) { return a + b; }, 0);

         var totalRow = document.getElementsByClassName('tc')[0];
         totalRow.classList.remove('hidden');
         var total = td[td.length-1];
         total.innerHTML = sum;

         // report a proportion:
         var nProp = (sum/n*100).toFixed(2) + "%";

           //information to extract:
           var intervalNo = document.getElementsByClassName('selected').length;

        // create another  tooltip for selection box:
                tt2.style("left", ((x1+x2)/4) + "px"); //positioning!
                tt2.style("top", tVal - (y1 - 30) + "px");
                tt2.style('visibility', 'visible');
                tt2.html("Interval Range: <span>" + intRange[0] + " - " + intRange[intRange.length-1] +
                        "</span> <br> Frequency: <span>" + sum +  "," + nProp + "</span> <br> No. of intervals: <span>" +
                       intervalNo + "</span>");

       }
    }
};

 //Reset Button:
   reset = function() {

     d3.select(panel).selectAll('polygon')
        .attr('class', 'histBar');

      var totalRow = document.getElementsByClassName('tc')[0];
      totalRow.classList.remove('hidden');

     for (i = 1; i <= count; i++) {

       var dataRow = document.getElementById('tr' + i);
       resetRowSelection(dataRow);
     };

       //reset total to n
       var total = td[td.length-1];
       total.innerHTML = n;

       //remove box:
    d3.select('#selectRect')
      .attr('points', '0,0')
      .attr("class", "selectRect");
      d3.select('#selection')
      .style('visibility', 'hidden');

    hideBox();

    viewTable.innerHTML = "View Table";
    table.classList.add('hidden');
    t = true;
 };

 // plotregion reset:
 var plotRegion = document.getElementsByTagName('rect')[1];
   plotRegion.addEventListener("click", reset, false);

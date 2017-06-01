//JavaScript code for histograms:
/* A histogram has code that is very similar to a one way bar plot
with boxplot properties. JSON data objects differ (class intervals). */

/* --------------------------------------------
  Table properties (Frequency distribution table):
  Includes identifying cells, rows, properties of
  the table, and the viewTable button.
---------------------------------------------*/

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

/* -------------------------------------------------------------
          Histogram bars and label properties:
Creating labels, identifying histogram bars, and assigning attributes
and properties to bars and labels.
---------------------------------------------------------------*/

var svg = document.getElementsByTagName('svg')[0];

//to expand plotRegion rectangle to show labels:
var rect = document.getElementsByTagName('rect')[2];
extendPlotRegion(rect);

var count = counts.length;

//get Grob object where hist bars lie:
var Grob = getGrob('hist');


// create labels for each hist bar:
for (i = 1; i<= count; i++) {
  gLabel(i);
}

for (i = 1; i <= count; i++) {
  gRect(i);
}

// Creating all the labels:
for (i  = 1; i <= count; i++) {

  var bar = document.getElementById(Grob + '.' + i);
  bar.setAttribute('class', 'histBar');

  var coords = bar.getAttribute('points');
  var small = coords.split(" ")[1];
  var sx = Number(small.split(",")[0]);
  var coordsxy = coords.split(" ")[2];
  var x = Number(coordsxy.split(",")[0]); //co-ordinates based upon SVG elements.
  var y = Number(coordsxy.split(",")[1]);
  label('label', 'Class range: ', i, 60);
  label('classLabel', intervals[i-1].toFixed(2) + ' - ' + intervals[i].toFixed(2), i, 45);
  label( 'countLabel','N = ' , i, 30);
  tLabel('countLabel',counts[i-1] + ', ' + (prop[i-1]*100).toFixed(2) + "%", i, document.getElementById('countLabel' + i));

      // Attach and draw rectangles to labels according to the size of the gLabel (with all labels attached)
         drawRectLabel(i);

};

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

/* ------------------------------------------------------------
      Histogram Interactivity - hovers, clicks on bars and in
      relation to table
---------------------------------------------------------------*/
//setting mouse events:
for (i = 1; i <= count; i++) {
  (function(i){
    var bar = document.getElementById(Grob + '.' + i);
    bar.addEventListener("mouseover",function(){light(i, 'light')},false);
    bar.addEventListener("mouseout", function(){normal(i, 'light')}, false);
    bar.addEventListener("click", function(){fade(i)}, false);
    }) (i)
  };

// on click:
 fade = function(i) {
   for (j = 1; j <= count; j ++) {

     var bar = document.getElementById(Grob + '.' + j);

     var gLabel = document.getElementById('gLabel' + j);
     var dataRow = document.getElementById('tr' + j);

     var totalRow = document.getElementsByClassName('tc')[0];
     totalRow.classList.add('hidden');

     if (i == j) {
       bar.setAttribute('class', 'histBar selected');
      gLabel.classList.remove('invisible');

      var l = bar.getAttribute('fill');
      var lp = l.substring(l.lastIndexOf("("), l.lastIndexOf(")"));

      returnRowSelection(lp, dataRow);

     } else {
       bar.setAttribute('class', 'histBar none');
       gLabel.classList.add('invisible');

       omitRowSelection(dataRow);

     }
 }
  hideBox();
 };

 /* ------------------------------------------------------------
      Facilitate selection box draggable by users:
 ---------------------------------------------------------------*/
var svg = document.getElementsByTagName('svg')[0];

//set container with no style padding:
var svgContainer = document.getElementById('svg-container');
svgContainer.classList.add('contained');

svg.setAttribute('draggable', 'false');

//create 'invisible' selection box that user will see when they begin to drag:
createSelectionBox(Grob);

var evt = window.event; // required for FF to work.
var zoomBox = {};

// assign mouse events:
svg.setAttribute('onmouseup', 'MouseUp(evt)');
svg.setAttribute('onmousemove', 'MouseDrag(evt)');
svg.setAttribute('onmousedown', 'MouseDown(evt)'); // defined below.

MouseDrag = function(evt) {
    if(zoomBox["isDrawing"]) {
        svg.style.cursor = "crosshair";
        zoomBox["endX"] = evt.pageX - 20;
        zoomBox["endY"] = evt.pageY - 20;

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
        var gLabel = document.getElementById('gLabel' + i);
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
              gLabel.classList.add('invisible');
            } else {
              //points that lie within the  boundary box drawn:
          if(((x1 <= bx && bx <= x2) && (x1 <= tx && tx <= x2)) && ((y1 <= by && by <= y2) && (y1 <= ty && ty <= y2))) {
            bar.setAttribute('class', ' histBar selected');
            l = bar.getAttribute('fill');
            lp = l.substring(l.lastIndexOf("("), l.lastIndexOf(")"));

            returnRowSelection(lp, dataRow);

            groupN.push(counts[i-1]); // store frequency from each hexbin that's selected

            //need  a way to compare intervals:
            intRange.push(intervals[i-1], intervals[i]);

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

         var selectedGroup = document.getElementById('selectionLabelGroup');
         if (selectedGroup !== null) {
             selectedGroup.remove();
            }

        //create group label by selection:
        var selectionLabelGroup = createSelectionLabelGroup();

           //information to extract:
           var intervalNo = document.getElementsByClassName('selected').length;

        //create labels:
        selectionLabel('range', (x1+x2)/2, y1 -15, 'Interval Range: ');
        tLabel('intRangeVal', intRange[0] + " - " + intRange[intRange.length-1], 0, document.getElementById('range'));

        selectionLabel('groupN', (x1+x2)/2, y1-30, 'Frequency: ');
        tLabel('groupNval', sum + ', ' + nProp, 0, document.getElementById('groupN'));

        selectionLabel('nIntervals', (x1+x2)/2, y1-45, 'No. of intervals: ');
        tLabel('nIntVal', intervalNo, 0, document.getElementById('nIntervals'));

        // Attach and draw rectangle to label:
        drawSelectRectLabel(selectionLabelGroup);

       }
    }
};


 //Reset Button:
   reset = function() {
     for (i = 1; i <= count; i++) {
       var bar = document.getElementById(Grob + "." + i);
       bar.setAttribute('class', 'histBar');

       var gLabel = document.getElementById('gLabel' + i);
       gLabel.classList.add('invisible');

       var dataRow = document.getElementById('tr' + i);
       resetRowSelection(dataRow);

       var totalRow = document.getElementsByClassName('tc')[0];
       totalRow.classList.remove('hidden');

       //reset total to n
       var total = td[td.length-1];
       total.innerHTML = n;

       //remove box:
       var selectRect = document.getElementById('selectRect');
       if (selectRect.getAttribute('points') !== null) {
         selectRect.removeAttribute('points');
    }
      var selectionLabelGroup = document.getElementById('selectionLabelGroup');
         if (selectionLabelGroup !== null || undefined) {
           selectionLabelGroup.remove();
         }
   }
    hideBox();

    viewTable.innerHTML = "View Table";
    table.classList.add('hidden');
    t = true;
 };

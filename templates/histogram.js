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
rect.setAttribute('class', 'rect');
rect.setAttribute('x', rect.getAttribute('x') -20);
rect.setAttribute('y', rect.getAttribute('y')-20);


//PARSING DATA:
var intervals  = JSON.parse(intervals);
var counts  = JSON.parse(counts);
var prop = JSON.parse(prop);
var boxData = JSON.parse(boxData);

var count = counts.length;

//identify polygon bars:
var p = document.getElementsByTagName('polygon')[2];
// to skip the first two polygons that correspond to the boxplot at the bottom
var id = p.getAttribute('id');
var Grob = id.substring(0, id.lastIndexOf('.'));


//creating group labels:
gLabel = function(i) {
    var gEl = document.createElementNS("http://www.w3.org/2000/svg", "g");
    gEl.setAttributeNS(null, 'id', 'gLabel' + i);
    gEl.setAttributeNS(null, 'class', 'gLabel invisible');
    var panel = document.getElementsByTagName('g')[0];
    panel.appendChild(gEl);
};

gRect = function(i) {
    var gRect = document.createElementNS("http://www.w3.org/2000/svg", "rect");
    gRect.setAttributeNS(null, 'id', 'gRect' + i);
    gRect.setAttributeNS(null, 'class', 'gRect');
    var gLabel = document.getElementById('gLabel' + i);
    gLabel.appendChild(gRect);
  };


for (i = 1; i<= count; i++) {
  gLabel(i);
}

for (i = 1; i <= count; i++) {
  gRect(i);
}

label = function(id, textinput, i, tf) {
//attributes for the text SVG element
  var label = document.createElementNS("http://www.w3.org/2000/svg", "text");
    label.setAttributeNS(null, 'id', id + i);
    label.setAttributeNS(null, 'class', 'label' + ' ' + id);
    label.setAttributeNS(null, 'transform', 'translate('+ ((x+sx)/2) + ', ' + (y + tf) +') scale(1, -1)');

// Creating the text label element:
  var textNode = document.createTextNode(textinput);
    label.appendChild(textNode);
    gLabel = document.getElementById('gLabel' + i);
    gLabel.appendChild(label);
};

//creating tspan labels - for customizing text in bold:
tLabel = function(id, textinput, i, lab) {
  var tLabel = document.createElementNS("http://www.w3.org/2000/svg", "tspan");
  tLabel.setAttributeNS(null, 'id', id + i);
  tLabel.setAttributeNS(null, 'class', 'tLabel');
  var textNode = document.createTextNode(textinput);
  tLabel.appendChild(textNode);
  lab.appendChild(tLabel);

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
        var gLabel = document.getElementById('gLabel' + i);
        rectParam = gLabel.getBBox();
        var gRect = document.getElementById('gRect' + i);
        gRect.setAttribute('x', rectParam.x-5);
        gRect.setAttribute('y', rectParam.y);
        gRect.setAttribute('width', rectParam.width+10);
        gRect.setAttribute('height', rectParam.height);

};

/* -------------------------------------------------------------
                  Box plot properties:
---------------------------------------------------------------- */

//Obtaining the 'polygon' boxes associated with the boxplot:
polygonBox = document.getElementsByTagName('polygon');
polygonId = polygonBox[0].id;
// this differs from dotplots - the boxplot appears to be drawn first...
idLine = polygonId.substring(0, polygonId.lastIndexOf('.'));

for (i = 1; i <= polygonBox.length; i ++) {
  if (polygonBox[i-1].id.indexOf(idLine) >= 0){
    polygonBox[i-1].setAttribute('class', 'box');
  }
}

//Min and Max - obtaining the ends of of the boxplot (lines):
polyLines = document.getElementsByTagName('polyline');
for (i = 1; i <= polyLines.length; i++) {
if (polyLines[i-1].id.indexOf('GRID') >= 0) {
  polyLines[i-1].setAttribute("class", "line");
}
};

var lines = document.getElementsByClassName("line");
var lastId = lines[lines.length-1].id;
var lastLine = lastId.substring(0, lastId.lastIndexOf('.'));

//functions to create boxLabels:

boxLabel = function(textinput) {

var boxLabel = document.createElementNS("http://www.w3.org/2000/svg", "text");
  boxLabel.setAttributeNS(null, 'id', textinput);
  boxLabel.setAttributeNS(null, 'class', 'label boxData hidden');
  boxLabel.setAttributeNS(null, 'transform', 'translate(' + Number(x) + ',' + (Number(y) + 10) + ') scale(1, -1)');

  var textNode = document.createTextNode(textinput);
  boxLabel.appendChild(textNode);
  var panel = document.getElementsByTagName('g')[0];
  panel.appendChild(boxLabel);
};

boxLabelSet = function(p, r, q, textinput) {
  if (textinput == "Min" ||  textinput == "Max") {
    line = document.getElementById(lastLine + '.' +  p);
    // p will either be 1 or 2 -> 1 = minLine, 2 = maxLine
    line.setAttribute('class', 'box');
    boxPoints = line.getAttribute('points').split(" ")[r].split(",");
  } else {
    box = document.getElementsByClassName('box')[p];
    // boxplot split into two boxes - lowerbox (p = 0) and upperbox (p = 1)
    boxPoints = box.getAttribute('points').split(" ")[r].split(",");
  }
  x = boxPoints[0];
  y = boxPoints[1];

  if (textinput == "Median") { // move median label below the box plot
   y = boxPoints[1] - 25;
  }

  text = textinput + ": " + boxData[q].quantiles;
  // this is associated with the boxData imported from R.
  boxLabel(text);
};

boxLabelSet(0, 1, 0,'LQ');
boxLabelSet(1, 2, 2, 'UQ');
boxLabelSet(1, 0, 1, 'Median');
boxLabelSet(1, 0, 3, 'Min');
boxLabelSet(2, 1, 4, 'Max');

/* -------------------------------------------------------
                  Box Plot interactions:
---------------------------------------------------------*/
var box = document.getElementsByClassName('box');
var boxData = document.getElementsByClassName('boxData');
var totalRow = document.getElementById('totalRow');

//setting interactions and colors for box plot:
for (i = 0; i < box.length; i++) {
  box[i].addEventListener('mouseover', fillBox, false);
  box[i].addEventListener('mouseout', normalBox, false);
  box[i].addEventListener('click', showBox, false);
}

//on hover:
function fillBox() {
  for (i = 0; i < box.length; i++) {
  box[i].classList.add('fillBox');
}
};

//hover out:
function normalBox() {
  for (i = 0; i < box.length; i++) {
    box[i].classList.remove('fillBox');
}
};

//on click:
function showBox() {
  for (i =0; i < boxData.length; i++) {
    boxData[i].classList.remove('hidden');
}
};


/* ------------------------------------------------------------
      Histogram Interactivity - hovers, clicks on bars and in
      relation to table
---------------------------------------------------------------*/
//setting mouse events:
for (i = 1; i <= count; i++) {
  (function(i){
    var bar = document.getElementById(Grob + '.' + i);
    bar.addEventListener("mouseover",function(){light(i)},false);
    bar.addEventListener("mouseout", function(){normal(i)}, false);
    bar.addEventListener("click", function(){fade(i)}, false);
    }) (i)
  };

//on hover:
 light = function(i) {
   var bar = document.getElementById(Grob + '.' + i);
      bar.classList.add('light');
   var gLabel = document.getElementById('gLabel' + i);
   gLabel.classList.remove('invisible');
 };

//on hover out:
 normal = function(i) {
   var bar = document.getElementById(Grob + '.' + i);
   bar.classList.remove('light');
   var gLabel = document.getElementById('gLabel' + i);
   gLabel.classList.add('invisible');
 };

// on click:
 fade = function(i) {
   for (j = 1; j <= count; j ++) {

     var bar = document.getElementById(Grob + '.' + j);
      var l = bar.getAttribute('fill');
      var lp = l.substring(l.lastIndexOf("("), l.lastIndexOf(")"));

     var gLabel = document.getElementById('gLabel' + j);
     var dataRow = document.getElementById('tr' + j);

     var totalRow = document.getElementsByClassName('tc')[0];
     totalRow.classList.add('hidden');

     if (i == j) {
       bar.setAttribute('class', 'histBar selected');
      gLabel.classList.remove('invisible');

       dataRow.setAttribute('class', 'rowSelect');
       dataRow.style.backgroundColor = "rgba" + lp + ", 0.3)";

     } else {
       bar.setAttribute('class', 'histBar none');
       gLabel.classList.add('invisible');

      dataRow.classList.remove('rowSelect');
      dataRow.classList.add('hidden');
      dataRow.style.backgroundColor = "white";
     }
 }
  for (k = 0; k < boxData.length; k++) {
    boxData[k].classList.add('hidden');
  }
 };

// Adding selection drag box for user to isolate a certain group of bars:
/* MULTISELECT ON MOUSE DRAG*/
var svg = document.getElementsByTagName('svg')[0];

//set container with no style padding:
var svgContainer = document.getElementById('svg-container');
svgContainer.classList.add('contained');

svg.setAttribute('draggable', 'false');

var rect = document.getElementsByTagName('rect');
var width = svg.width.baseVal.value;
var height = svg.height.baseVal.value;

//putting selection rectangle in a group element:
var g = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  g.setAttributeNS(null, 'id', 'selectionBox');
  var panel = document.getElementById(Grob);
  panel.appendChild(g);


var evt = window.event;

svg.setAttribute('onmouseup', 'MouseUp(evt)');
svg.setAttribute('onmousemove', 'MouseDrag(evt)');
svg.setAttribute('onmousedown', 'MouseDown(evt)');

  var selectRect = document.createElementNS('http://www.w3.org/2000/svg', 'polygon');
  selectRect.setAttributeNS(null, 'id', 'selectRect');
  selectRect.setAttributeNS(null, 'class', 'selectRect');
  g.appendChild(selectRect);


  //create a new label:
  selectionLabel = function(id, x, y, textinput) {
  var selectionLabel = document.createElementNS("http://www.w3.org/2000/svg", 'text');
       selectionLabel.setAttributeNS(null, 'class', 'label');
       selectionLabel.setAttributeNS(null, 'id', id);
       selectionLabel.setAttributeNS(null, 'transform', 'translate(' + x +  ',' + y + ') scale(1, -1)');
      var text = document.createTextNode(textinput);
      selectionLabel.appendChild(text);

      var selectionLabelGroup = document.getElementById('selectionLabelGroup');
       selectionLabelGroup.appendChild(selectionLabel);
     }

var zoomBox = {};

MouseDown = function(evt) {

  var selectedGroup = document.getElementById('selectionLabelGroup');
  if (selectedGroup !== null) {
      selectedGroup.remove();
     }

    zoomBox["startX"] = evt.pageX - 20;
    zoomBox["startY"] = evt.pageY -20;
    zoomBox["isDrawing"] = true;
   selectRect.setAttribute('points',  zoomBox["startX"] + ',' + zoomBox["startY"]);
};

MouseUp = function(evt) {
  svg.style.cursor = "default";
      zoomBox["endX"] = evt.pageX -20;
      zoomBox["endY"] = evt.pageY -20;
      zoomBox["isDrawing"] = false;

  };

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

            dataRow.classList.remove('hidden');
            dataRow.classList.add('rowSelect');
            dataRow.style.backgroundColor = "rgba" + lp + ", 0.25)";

            groupN.push(counts[i-1]); // store frequency from each hexbin that's selected

            //need  a way to compare intervals:
            intRange.push(intervals[i-1], intervals[i]);
          //  console.log(intRange);

           } else {
             bar.setAttribute('class', 'histBar none');

             dataRow.classList.add('hidden');
             dataRow.classList.remove('rowSelect');
             dataRow.style.backgroundColor = "white";
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
        //grouping it together!:
        var selectionLabelGroup = document.createElementNS("http://www.w3.org/2000/svg", 'g');
            selectionLabelGroup.setAttributeNS(null, 'class', 'gLabel');
            selectionLabelGroup.setAttributeNS(null, 'id', 'selectionLabelGroup');
            var panel = document.getElementsByTagName('g')[0];
            panel.appendChild(selectionLabelGroup);
        //make a rectangle for this special label:
        var selectionLabelRect = document.createElementNS("http://www.w3.org/2000/svg", 'rect');
            selectionLabelRect.setAttributeNS(null, 'class', 'gRect');
            selectionLabelRect.setAttributeNS(null, 'id', 'selectionLabelRect');
            selectionLabelGroup.appendChild(selectionLabelRect);

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
          var sRectParam = selectionLabelGroup.getBBox();
          selectionLabelRect.setAttribute('x', sRectParam.x-5);
          selectionLabelRect.setAttribute('y', sRectParam.y);
          selectionLabelRect.setAttribute('width', sRectParam.width+10);
          selectionLabelRect.setAttribute('height', sRectParam.height);

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
       dataRow.classList.remove('hidden');
       dataRow.classList.remove('rowSelect');
       dataRow.style.backgroundColor = "white";

       var totalRow = document.getElementsByClassName('tc')[0];
       totalRow.classList.remove('hidden');

       //reset total to n
       var total = td[td.length-1];
       total.innerHTML = n;

       //remove box:
       var selectRect = document.getElementById('selectRect');
       var selectionLabelGroup = document.getElementById('selectionLabelGroup');
       if (selectRect.getAttribute('points') !== null) {
         selectRect.removeAttribute('points');
         selectionLabelGroup.remove();
       }
   }
    for (k = 0; k < boxData.length; k++) {
      boxData[k].classList.add('hidden');
    }
    viewTable.innerHTML = "View Table";
    table.classList.add('hidden');
    t = true;
 };

//JavaScript code for histograms:
/* A histogram has code that is very similar to a one way bar plot
with boxplot properties. JSON data objects differ (class intervals). */

/* --------------------------------------------
  Table properties (Frequency distribution table):
  Includes identifying cells, rows, properties of
  the table, and the viewTable button.
---------------------------------------------*/

var table = document.getElementById('table');

//no. of rows in table
var nrow = table.rows.length;

//no. of columns in table
var ncol = document.getElementsByTagName('th').length;

var td = document.getElementsByTagName('td');
var cellNo = td.length;
var i;

for (i = 1; i <= cellNo; i++) {
  td[i-1].setAttribute('id', 'td' + i);
};

//no. of rows in table
var tr = document.getElementsByTagName('tr');
for (i = 1; i < nrow; i++) {
  tr[i].setAttribute('id', 'tr' + i);
};

var totalRow = tr[nrow-1];
totalRow.setAttribute('class', 'tc');

//drive the viewTable button:
  viewTable = document.getElementById('viewTable');
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
var intervals  = JSON.parse(intervals)
var counts  = JSON.parse(counts)
var prop = JSON.parse(prop);
var boxData = JSON.parse(boxData);

var count = counts.length;

//identify polygon bars:
var p = document.getElementsByTagName('polygon')[2];
// to skip the first two polygons that correspond to the boxplot at the bottom
var id = p.getAttribute('id');
var Grob = id.substring(0, id.lastIndexOf('.'));
var panel = document.getElementsByTagName('g')[0];

var i;

//creating group labels:
gLabel = function (Grob, i) {
    'use strict';
    var panel = document.getElementById(Grob), gEl = document.createElementNS("http://www.w3.org/2000/svg", "g");
    gEl.setAttributeNS(null, 'id', 'gLabel' + i);
    gEl.setAttributeNS(null, 'class', 'gLabel invisible');
    panel.appendChild(gEl);
};

gRect = function (i) {
    var gRect = document.createElementNS("http://www.w3.org/2000/svg", "rect");
    gRect.setAttributeNS(null, 'id', 'gRect' + i);
    gRect.setAttributeNS(null, 'class', 'gRect');
    var gLabel = document.getElementById('gLabel' + i);
    gLabel.appendChild(gRect);
  };


for (i in count) {
  gLabel(Grob, i);
}

for (i in count) {
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
tLabel = function(id, textinput, i) {
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
  var lab = document.getElementById('countLabel' + i);
  tLabel('countLabel',counts[i-1] + ', ' + (prop[i-1]*100).toFixed(2) + "%", i);

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
box[i].setAttribute('onmouseover', 'fillBox()');
box[i].setAttribute('onmouseout', 'normalBox()');
box[i].setAttribute('onclick', 'showBox()');
}

//on hover:
fillBox = function() {
  for (i = 0; i < box.length; i++) {
  box[i].classList.add('fillBox');
}
};

//hover out:
normalBox = function() {
  for (i = 0; i < box.length; i++) {
    box[i].classList.remove('fillBox');
}
};

//on click:
showBox = function() {
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
 var bar = document.getElementById(Grob + '.' + i);
 bar.setAttribute('onmouseover', 'light('+ i + ')');
 bar.setAttribute('onmouseout', 'normal(' + i +')');
 bar.setAttribute('onclick', 'fade(' + i +')');
 }

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
   }
    for (k = 0; k < boxData.length; k++) {
      boxData[k].classList.add('hidden');
    }
    viewTable.innerHTML = "View Table";
    table.classList.add('hidden');
    t = true;
 };

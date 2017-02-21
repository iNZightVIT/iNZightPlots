/* JavaScript code for dot plots and scatterplots (inclusive of iNZightMaps):
Code is split into 3 sections: table properties,
                                interactions,
                                selectionCanvas (for multi-selection of points).
  Issues: labels do not automatically resize when variables are hidden
           Cross-browser compatibility
  Browser Compatibility: Google Chrome 56.0, Safari 10.2,
                        FireFox (except for multi-select)
   Labels appear for Edge, IE11, IE10. Table selection needs fixing. */

/* -----------------------------------------------------
                Table properties:
Code to assign classes, ids to table cells and rows to
link up to interactions on the plot.
Creation of HTML form/select to allow user to select
the variables to display in labels and in the table.
-------------------------------------------------------- */

var table = document.getElementById('table');
table.style.padding = "20px";
table.style.display = "none";

//no. of rows in table
var nrow = document.getElementById('table').rows.length;

//no. of columns in table
var ncol = document.getElementsByTagName('th').length;

var td = document.getElementsByTagName('td');
cellNo = td.length;

for (i = 1; i <= cellNo; i ++) {
  td[i-1].setAttribute('align', 'center');
  td[i-1].setAttribute('id', i);
};

for (j = 1; j <= ncol; j++) {

  th = document.getElementsByTagName('th');
  th[j-1].style.textAlign = "center";
  th[j-1].setAttribute('class', j-1);

  for (i=1; i <= cellNo; i++) {
  var td = document.getElementsByTagName('td');
    if (i%ncol == j) {
      td[i].setAttribute('class', j);
    }
  }
};

//no. of rows in table
nrow = document.getElementById('table').rows.length;
for (i = 1; i < nrow; i ++) {
  var tr = document.getElementsByTagName('tr');
  tr[i].setAttribute('id', 'tr' + i);
  tr[i].setAttribute('align', 'center');
};

//  Select option for interactivity: to select variables accordingly
var form = document.createElement('form');
form.setAttribute('class', 'form-inline');
form.setAttribute('id', 'form');
form.style.display = "inline";
form.style.padding = "20px";
document.getElementById('control').appendChild(form);

var selectVar = document.createElement('select');
selectVar.setAttribute('class', 'form-control');
selectVar.setAttribute('id', 'selectVar');
selectVar.setAttribute('onchange', 'selected()');
selectVar.setAttribute('multiple', 'multiple');
form.appendChild(selectVar);


//Creating options relative to table generated:
for (i = 0; i<=ncol-1; i++){
    var opt = document.createElement('option');
    opt.value = i;
    opt.innerHTML = th[i].innerHTML;
    selectVar.appendChild(opt);
    if (opt.value == 0 || opt.value == undefined) {
      opt.value = 0;
      opt.style.fontWeight = "bold";
      opt.innerHTML = "Variables to display";
      opt.selected = "selected";
    }
};

//drive the viewTable button:
  var viewTable = document.getElementById('viewTable');
  t = true;
showTable = function() {
  if(t) {
    viewTable.innerHTML = "Hide Table";
    table.style.display =  "table";
    t = false;
  } else {
    viewTable.innerHTML = "View Table";
    table.style.display = "none";
    t = true
  }
};

/* -------------------------------------------------------
              dotplot and scatterplot interactions
      Code to assign points (and hide certain things),
      create svg labels for each point, boxplot labels,
      drive interaction (hovers, clicks, reset).
      Data is also parsed.
      Select option interaction.
---------------------------------------------------------- */

//Originally there was padding - but it affects the selectionBox transformations.
var svg = document.getElementsByTagName('svg')[0];
svg.setAttribute('preserveAspectRatio', 'xMinYMin meet');
//set container with no style padding:
var svgContainer = document.getElementById('svg-container');
svgContainer.setAttribute('style', 'margin-left:0; padding:0; margin-right:0;');

//to expand plotRegion rectangle to show labels:
var rect = document.getElementsByTagName('rect')[0];
rect.setAttribute('width', rect.getAttribute('width')*2);
rect.setAttribute('height', rect.getAttribute('height')*2);
rect.setAttribute('x', 0);
rect.setAttribute('y', 0);

//PARSING Data:
var names = JSON.parse(names);
var tableData = JSON.parse(tableData);
varNo = names.length + 1;


if (boxData != undefined) {
  /* --------------------------------------------------
  Box plot labels and interactions - for dotplots only!
  --------------------------------------------------- */
  var boxData = JSON.parse(boxData);
  var Grob = "DOTPOINTS.1";
  count = document.getElementById(Grob).childElementCount;
  var panel = document.getElementById(Grob);

  //BOX PLOT LABELS:
  /* The box plot is made up of 2 lines (line that extends to the minimum, and the other extending to the maximum)
  and two 'polygon' rectangles that make up the box (a lower box that draws up to the median, while the upper draws from the median to the upper quartile value).
  */

  //Obtaining the 'polygon' boxes associated with the boxplot:
  var polygonBox = document.getElementsByTagName('polygon');
  var polygonId = polygonBox[polygonBox.length -1].id;
  var idLine = polygonId.substring(0, polygonId.lastIndexOf('.'));

  for (i = 1; i <= polygonBox.length; i ++) {
    if (polygonBox[i-1].id.indexOf(idLine) >= 0){
      polygonBox[i-1].setAttribute('class', 'box');
    }
  }

  //Min and Max - obtaining the ends of of the boxplot (lines): these are identified as the last two lines in the 'polyline' group.
  var polyLines = document.getElementsByTagName('polyline');
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
// Note: could possibly shorten this and call a single text label function for the whole document.
  var boxLabel = document.createElementNS("http://www.w3.org/2000/svg", "text");
    boxLabel.setAttributeNS(null, 'x', '0');
    boxLabel.setAttributeNS(null, 'y', '0');
    boxLabel.setAttributeNS(null, 'font-size', "12");
    boxLabel.setAttributeNS(null, 'fill', 'black');
    boxLabel.setAttributeNS(null, 'fill-opacity', '1');
    boxLabel.setAttributeNS(null, 'text-anchor', 'middle');
    boxLabel.setAttributeNS(null, 'visibility', 'hidden');
    boxLabel.setAttributeNS(null, 'transform', 'translate(' + Number(x) + ',' + (Number(y) + 10) + ') scale(1, -1)'); //hardcoded!
    boxLabel.setAttributeNS(null, 'id', textinput);
    boxLabel.setAttributeNS(null, 'class', 'boxData');

    var textNode = document.createTextNode(textinput);

      boxLabel.appendChild(textNode);
      panel.appendChild(boxLabel);
  };

  boxLabelSet = function(p, r, q, textinput) {
    if (textinput == "Min" ||  textinput == "Max") {
      var line = document.getElementById(lastLine + '.' +  p); // p will either be 1 or 2 -> 1 = minLine, 2 = maxLine
      line.setAttribute('class', 'box');
      var boxPoints = line.getAttribute('points').split(" ")[r].split(",");
    } else {
      var box = document.getElementsByClassName('box')[p]; // boxplot split into two boxes - lowerbox (p = 0) and upperbox (p = 1)
      var boxPoints = box.getAttribute('points').split(" ")[r].split(",");
    }
    x = boxPoints[0];
    y = boxPoints[1];

    if (textinput == "Median") { // move median label below the box plot
     y = boxPoints[1] - 25;
    }
    
    text = textinput + ": " + boxData[q].quantiles; // this is associated with the boxData imported from R. q = 0 (LQ), 1 (UQ), 2 (Median), 3 (Min), 4 (Max)
    boxLabel(text);
  };

  boxLabelSet(0, 1, 0,'LQ');
  boxLabelSet(1, 2, 2, 'UQ');
  boxLabelSet(1, 0, 1, 'Median');
  boxLabelSet(1, 0, 3, 'Min');
  boxLabelSet(2, 1, 4, 'Max');


  //Box Plot interactions:
  box = document.getElementsByClassName('box');

  //setting interactions and colors for box plot:
  for (i = 0; i < box.length; i++) {
  box[i].setAttribute('fill', 'gray');
  box[i].setAttribute('fill-opacity', '0.5');
  box[i].setAttribute('onmouseover', 'fillBox()');
  box[i].setAttribute('onmouseout', 'normalBox()');
  box[i].setAttribute('onclick', 'showBox()');
  }

  fillBox = function() {
    for (i = 0; i < box.length; i++) {
    box[i].setAttribute('fill-opacity', '0.3');
  }
  };

  normalBox = function() {
    for (i = 0; i < box.length; i++) {
      box[i].setAttribute('fill-opacity', '0.5');
  }
  };

  showBox = function() {
    var boxData = document.getElementsByClassName('boxData');
    for (i =0; i < boxData.length; i++) {
      boxData[i].setAttribute('visibility', 'visible');
  }
  }

} else {
  var Grob = "SCATTERPOINTS.1";
  var count = document.getElementById(Grob).childElementCount;
}

var panel = document.getElementsByTagName('g')[0];

//POINT LABELS:
//function to create g elements to group labels together:
gLabel = function(i) {

var gEl = document.createElementNS("http://www.w3.org/2000/svg", "g");
    gEl.setAttributeNS(null, 'id', 'gLabel' + i);
    gEl.setAttributeNS(null, 'visibility', 'hidden');
    panel.appendChild(gEl);
  };

//function to create rectangles for labels:
  gRect = function(i) {
    var gRect = document.createElementNS("http://www.w3.org/2000/svg", "rect");
        gRect.setAttributeNS(null, 'visibility', 'inherit');
        gRect.setAttributeNS(null, 'id', 'gRect' + i);
        gRect.setAttributeNS(null,'fill', 'white');
        gRect.setAttributeNS(null,'fill-opacity', '0.8');
        gRect.setAttributeNS(null,'rx', '5');
        gRect.setAttributeNS(null, 'ry', '5');
        gRect.setAttributeNS(null, 'stroke', 'lightgray');
    gLabel = document.getElementById('gLabel' + i);
    gLabel.appendChild(gRect);
  };

//making rectangles and g elements for each point:
for (i = 1; i <= count; i++) {
  gLabel(i);
};

for (i = 1; i <= count; i++) {
  gRect(i);
}

//function to create text labels for scatterpoints:
label = function(id, textinput, i) {

var label = document.createElementNS("http://www.w3.org/2000/svg", "text");
  label.setAttributeNS(null, 'x', '0');
  label.setAttributeNS(null, 'y', '0');
  label.setAttributeNS(null, 'font-size', "12");
  label.setAttributeNS(null, 'fill', 'black');
  label.setAttributeNS(null, 'fill-opacity', '1');
  label.setAttributeNS(null, 'text-anchor', 'right');
  label.setAttributeNS(null, 'visibility', 'inherit');
  label.setAttributeNS(null, 'transform', 'translate(' + Number(x) + ',' + (Number(y) + varNo*15) + ') scale(1, -1)'); //hardcoded!
  label.setAttributeNS(null, 'id', id + i);

  var textNode = document.createTextNode(textinput);

    label.appendChild(textNode);
    var gLabel = document.getElementById('gLabel' + i);
    gLabel.appendChild(label);

};

//creating tspan labels - for customizing text in bold:
tLabel = function(id, textinput, i) {
  var tLabel = document.createElementNS("http://www.w3.org/2000/svg", "tspan");
  tLabel.setAttributeNS(null, 'visibility', 'inherit');
  tLabel.setAttributeNS(null, 'id', id + i);
  tLabel.style.fontWeight = "bold";

  textNode = document.createTextNode(textinput);
  tLabel.appendChild(textNode);
  lab.appendChild(tLabel);

};

//Create number and value labels:
for (j = 0; j < names.length; j++) {

for (i  = 1; i <= count; i++) {

  var point = document.getElementById(Grob + '.' + i);
  var x = point.getAttribute('x');
  var y = point.getAttribute('y');
  var textNo = 'No: ' + i;
  label('labelNo', textNo, i);
  text = [];
  text[j] = names[j] + ": ";
  label('label' + '.' + (j+1) + '.' , text[j], i);
  p = 'translate(' + Number(x) + ',' + (Number(y) + 15*(varNo-j-1)) + ') scale(1, -1)';
  lab = document.getElementById('label' + '.' + (j+1) + '.' + i);
  tLabel('tLabel', tableData[i-1][names[j]], i);
  lab.setAttribute('transform', p);
  lab.setAttribute('class', (j+1));

// Attach and draw rectangles to labels according to the size of the gLabel (with all labels attached)
  var gLabel = document.getElementById('gLabel' + i);
  rectParam = gLabel.getBBox(); // possibly need to state that if this does not work on the browser (as it only works on Chrome, Firefox, Safari), skip this code.
  var gRect = document.getElementById('gRect' + i);
  gRect.setAttribute('x', rectParam.x-1);
  gRect.setAttribute('y', rectParam.y-2);
  gRect.setAttribute('width', rectParam.width+2);
  gRect.setAttribute('height', rectParam.height+2);

  }
};


/// INTERACTION CODE: Hovers, Clicks, Legends
//Hovers, clicks on points to show labels and data from table:
for (i =1; i <= count; i++) {
  point = document.getElementById(Grob + "." + i);
  point.style.stroke = point.getAttribute('stroke');
  point.setAttribute('onmouseover', 'show(' + i + ')');
  point.setAttribute('onmouseout', 'normal(' + i + ')');
  point.setAttribute('onclick', 'info(' + i + ')');
};

//Hover on:
show = function(i) {
  var point = document.getElementById(Grob + "." + i);
  point.style.fill = point.getAttribute('stroke');
  point.setAttribute('stroke-width', point.getAttribute('stroke-width')*2);
  point.style.fillOpacity = "1";

  var gLabel = document.getElementById('gLabel' + i);
  gLabel.setAttribute('visibility', 'visible');
};

//Hover out:
normal = function(i) {
  var point = document.getElementById(Grob + "." + i);
  point.style.fill = "none";
  point.setAttribute('stroke-width', point.getAttribute('stroke-width')/2);

  var gLabel =document.getElementById('gLabel' + i);
  gLabel.setAttribute('visibility', 'hidden');

};

//On click:
info = function(i) {
  for (j = 1; j <= count; j++) {
    point = document.getElementById(Grob + "." + j);
    gLabel = document.getElementById('gLabel' + j);

    l = point.getAttribute('stroke');
    lp = l.substring(l.lastIndexOf("("), l.lastIndexOf(")"));

    dataRow = document.getElementById('tr' + j);

    if (i == j) {
      gLabel.setAttribute('visibility', 'visible');
      point.style.opacity = "1";

      dataRow.style.backgroundColor = "rgba" + lp + ", 0.25)";
      dataRow.style.display = "table-row";
      dataRow.style.opacity = "1";

    } else {
      gLabel.setAttribute('visibility', 'hidden');
      point.style.opacity = "0.3";
      dataRow.style.backgroundColor = "white";
      dataRow.style.display = "none";
    }
  }
  if (boxData != undefined) {
 boxData = document.getElementsByClassName('boxData');
  for (i =0; i < boxData.length; i++) {
    boxData[i].setAttribute('visibility', 'hidden');
  }
}
};

//LEGEND INTERACTION:


if (colGroupNo != (0 || undefined)) { // if there is a legend, colGroupNo should be a value
//grabbing keys and text from the legend:
var keys = document.getElementsByTagName('use');
var text = document.getElementsByTagName('text');

//assigning mouse events:
for (i = 1; i <= colGroupNo; i ++) { //colGroupNo -> colby levels from R (nlevels)
  var key = document.getElementById(keys[i-1].id);
  key.setAttribute('onmouseover', 'inner(' + i +')');
  key.setAttribute('onmouseout', 'out(' + i + ')');
  key.setAttribute('onclick', 'subset(' + i + ')');
  var keyText = document.getElementById(text[i+3].id);
  if (Grob == "DOTPOINTS.1") { // for dot plots - legend text differs
    var keyText = document.getElementById(text[i+2].id);
  }
  keyText.setAttribute('onmouseover', 'inner(' + i +')');
  keyText.setAttribute('onmouseout', 'out(' + i +')');
  keyText.setAttribute('onclick', 'subset(' + i + ')');
};

// hover on a legend group:
 inner = function(i) {
  var keyText = document.getElementById(text[i+3].id);
  if (Grob == "DOTPOINTS.1") {
    var keyText = document.getElementById(text[i+2].id);
  }
  var key = document.getElementById(keys[i-1].id);
  keyText.setAttribute('style', 'fill:' + key.getAttribute('fill'));
  keyText.setAttribute('font-size', '115%');
  key.setAttribute('fill-opacity', '0.5');
};

//hover out:
out = function(i) {
  var keyText = document.getElementById(text[i+3].id);
  if (Grob == "DOTPOINTS.1") {
    var keyText = document.getElementById(text[i+2].id);
  }
  var key = document.getElementById(keys[i-1].id);
  keyText.setAttribute('style', 'fill: black');
  keyText.setAttribute('font-size', '100%');
  key.setAttribute('fill-opacity', '1');
};

//on click, subsetting occurs:
subset = function(i) {
  for (j = 1; j <= count; j++) {
    var point = document.getElementById(Grob + '.' + j);
    var key = document.getElementById(keys[i-1].id);
    var label = document.getElementById('gLabel' + j);
    var dataRow = document.getElementById('tr' + j);
    label.setAttribute('visibility', 'hidden');

if (key.getAttribute('fill') == point.getAttribute('stroke')) {
  point.setAttribute('visibility', 'visible');
  point.setAttribute('opacity', '1');
  dataRow.style.display = "table-row";
  dataRow.style.backgroundColor = "white";
} else {
    point.setAttribute('visibility', 'hidden');
    dataRow.style.display = "none";
}
}
}
};


/* Link to interactive table + labels - "Variables to display" select/option box:
- may rewrite this in jQuery for detachment.
Note - maybe a shortcut using CSS where you can show/hide using classes which
could possibly increase speed?
*/

selected = function() {
sOpt = selectVar.selectedOptions; // this does not work on IE, and not fully supported. May need to replace this.
s = [];
for (i =0; i < sOpt.length; i++) {
  s.push(sOpt[i].value);
};

for (i =1; i <= ncol-1; i++) {
  var column = document.getElementsByClassName(i);
  var labels = svg.getElementsByClassName(i);

  for(j = 1; j <= column.length; j++) {
    if (s.indexOf('0') >= 0) {
      column[j-1].style.display = "table-cell";
    } else {
    column[j-1].style.display = "none";
    if (j <= labels.length) {
    labels[j-1].style.display = "none";
    labels[j-1].visibility = "hidden";
  }
}
}
};

for (i=0; i <= s.length; i++) {
  if (s[i] != undefined) {
    column = table.getElementsByClassName(s[i]);
    labels = svg.getElementsByClassName(s[i]);
    for (j = 1; j <= column.length; j++) {
      column[j-1].style.display = "table-cell";
      if (j <= labels.length) {
     labels[j-1].style.display = "inherit";
     labels[j-1].visibility = "inherit";
  }
}
}
}
};


/* --------------------------------------------------------------
                selectionCanvas.js

Code to select over a group of points via mouse drag.
//issues: breaks when user zooms, or when document
had padding to SVG.
ISSUES: Does not support FF, IE. - may rewrite in jQuery.
- Note: this may need to be revised as foreignObjects are not
supported in IE...

----------------------------------------------------------------- */

width = svg.width.baseVal.value;
height = svg.height.baseVal.value;

//Need to create canvas in order to draw rectangle on an svg element: - it requires a foreignObject. - NOT supported on IE!
var foreignObject = document.createElementNS('http://www.w3.org/2000/svg', 'foreignObject');
    foreignObject.setAttributeNS(null, 'id', 'foreignObject');
    foreignObject.setAttributeNS(null,'width', width);
    foreignObject.setAttributeNS(null, 'height', height);
    foreignObject.setAttributeNS(null, 'x', '0');
    foreignObject.setAttributeNS(null, 'y', '0');
    // The foreignObject containing the canvas is set to hidden to prevent it from affecting other mouse events.
    //Visibility is turned on when the user drags as written below (enables the user to draw a selection box over the plot.)
    foreignObject.setAttributeNS(null, 'visibility', 'hidden');
    svg.appendChild(foreignObject);


// creation of canvas element:
var canvas = document.createElementNS('http://www.w3.org/1999/xhtml', 'xhtml:canvas');
  canvas.setAttributeNS(null, 'id', 'canvas');
  canvas.setAttributeNS(null, 'width', width);
  canvas.setAttributeNS(null, 'height', height);
  canvas.setAttributeNS(null, 'x', '0');
  canvas.setAttributeNS(null, 'y', '0');
  foreignObject.appendChild(canvas);

//Get canvas context to draw rectangles according to mouse events:
var ctx = canvas.getContext("2d");
ctx.strokeStyle = "none";
ctx.fillStyle = "rgba(112,112,112, 0.25)";

var isDrawing = false;

//mouse events attached to svg element
svg.setAttribute('onmousedown', 'MouseDown(event)');
svg.setAttribute('onmouseup', 'MouseUp(event)');
svg.setAttribute('onmousemove', 'MouseMove(event)');

var selectBox = {};

//When the user begins to draw the rectangle...
function MouseDown(e) {
  var e = window.event || e;
  canvas.style.cursor = "crosshair";
  selectBox["isDrawing"] = true;
    selectBox["startX"] = e.pageX;
    selectBox["startY"] = e.pageY;
  };

// What happens when the user moves the mouse...
function MouseMove(e) {
  var e = window.event || e;
if(selectBox["isDrawing"]) {
  //when drawing - foreignObject visibility is on, to allow user to draw canvas rectangle.
  foreignObject.setAttribute('visibility', 'visible');
  window.scrollTo(0, 0);
    selectBox["endX"] = e.pageX;
    selectBox["endY"] = e.pageY;
		ctx.clearRect(0, 0, canvas.width, canvas.height);
		ctx.beginPath();
		ctx.rect(selectBox["startX"], selectBox["startY"], selectBox["endX"]-selectBox["startX"], selectBox["endY"]-selectBox["startY"]);
    ctx.fill();


    //Because the y-axis is inverted in the plot - need to invert the scale
     tVal = document.getElementsByTagName('g')[0].getAttribute('transform').substring(13, 16);

     //Because canvas can draw rectangles in any position (positive and negative!) - to calculate positions:
    if(selectBox["startX"] < selectBox["endX"]) {
    var x1 = selectBox["startX"];
    var x2 = selectBox["endX"];
  } else {
    var x1 = selectBox["endX"];
    var x2 = selectBox["startX"];
  }

  if (selectBox["startY"] < selectBox["endY"]) {
    var y1 = tVal - selectBox["startY"] - (selectBox["endY"]-selectBox["startY"]);
    var y2 = y1 + (selectBox["endY"]-selectBox["startY"]);
  } else {
    var y1 = tVal - selectBox["endY"] - (selectBox["startY"]-selectBox["endY"]);
    var y2 = y1 + (selectBox["startY"]-selectBox["endY"]);
  }

    for (i =1; i <= count; i++) {
      point = document.getElementById(Grob + '.' + i);
      gLabel = document.getElementById('gLabel' + i);
      dataRow = document.getElementById('tr' + i);

      x = point.x.baseVal.value;
      y = point.y.baseVal.value;

      if (point.getAttribute('visibility') != 'hidden') { // condition run on subsetted group

      //Condition run - where if the point lies within the rectangle selection box drawn:
      if((x1 <= x && x <= x2) && (y1 <= y && y <= y2))  {
         point.setAttribute('class', 'selected');
         l = point.getAttribute('stroke');
         lp = l.substring(l.lastIndexOf("("), l.lastIndexOf(")"));
         point.style.opacity = "1";
         gLabel.setAttribute('visibility', 'visible');
         dataRow.style.backgroundColor = "rgba" + lp + ", 0.25)";
         dataRow.style.display = "table-row";

       } else { // hides points if it's not in the drawn region
         point.setAttribute('class', 'none');
         gLabel.setAttribute('visibility', 'hidden');
         point.style.opacity = "0.3";
        dataRow.style.backgroundColor = "white";
         dataRow.style.display = "none";
       }
	} else {  // those that are hidden, remain hidden
    point.setAttribute('visibility', 'hidden');
    gLabel.setAttribute('visibility', 'hidden');
  }
}
}
};

//MouseUp - what happens after the user finishes drawing the rectangle.
function MouseUp(e) {
  var e = window.event || e;
	canvas.style.cursor = "default";
  selectBox["endX"] = e.pageX;
  selectBox["endY"] = e.pageY;
  selectBox["isDrawing"] = false;
  foreignObject.setAttribute('visibility', 'hidden'); // hides the foreignObject.
}


/* -------------------------------------------------
      Reset button - attempts to return to original state
-------------------------------------------------- */
//Reset Button:
  reset = function() {
    for (i = 1; i <= count; i++) {
    var point = document.getElementById(Grob + "." + i);
      point.style.opacity = '1';
      point.setAttribute('visibility', 'visible');
    var gLabel = document.getElementById('gLabel' + i);
      gLabel.setAttribute('visibility', 'hidden');
    var dataRow = document.getElementById('tr' + i);
      dataRow.style.display = "table-row";
      dataRow.style.backgroundColor = "white";
      dataRow.style.opacity = "1";


    var selectRect = document.getElementById('selectRect');
    if (selectRect != undefined) {
      selectRect.setAttribute('x', 0);
      selectRect.setAttribute('y', 0);
      selectRect.setAttribute('width', 0);
      selectRect.setAttribute('height', 0);
    }

    ctx.clearRect(0, 0, canvas.width, canvas.height);
    foreignObject.setAttribute('visibility', 'hidden');
  }
for (i =1; i <= ncol-1; i++) {
    column = table.getElementsByClassName(i);
    for (j = 1; j <=column.length; j++) {
    column.style.display = "table-cell";
  }
}
selectedVar.selectedIndex = "0";
};

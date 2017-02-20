//JavaScript code for histograms:
// @author: Yu Han Soh
/* A histogram has code that is very similar to a one way bar plot
with boxplot properties. JSON data objects differ (class intervals). */

/* --------------------------------------------
  table stuff - frequency distribution table
---------------------------------------------*/

var table = document.getElementById('table');
table.style.padding = "20px";
table.style.display = "none";

//no. of rows in table
nrow = document.getElementById('table').rows.length;

//no. of columns in table
ncol = document.getElementsByTagName('th').length;

var td = document.getElementsByTagName('td');
cellNo = td.length;

for (i = 1; i <= cellNo; i ++) {
  td[i-1].setAttribute('align', 'center');
  td[i-1].setAttribute('id', 'td' + i);
};

for (j = 1; j <= ncol; j++) {

  th = document.getElementsByTagName('th');
  th[j-1].style.textAlign = "center";
  th[j-1].setAttribute('class', j);

  };

//no. of rows in table
nrow = document.getElementById('table').rows.length;
  tr = document.getElementsByTagName('tr');
for (i = 1; i < nrow; i ++) {
  tr[i].setAttribute('id', 'tr' + i);
  tr[i].setAttribute('align', 'center');
};

totalRow = document.getElementsByTagName('tr')[nrow-1];
totalRow.setAttribute('id', 'totalRow');
totalRow.style.fontWeight = "bold";

//drive the viewTable button:
  viewTable = document.getElementById('viewTable');
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

/* -------------------------------------------------------------
          Creating labels for histogram:
---------------------------------------------------------------*/


var svg = document.getElementsByTagName('svg')[0];
svg.setAttribute('width', '100%'); // change to 100% for full width + responsive.
svg.setAttribute('height', '100%');

//hide rectangles:
//rectangle = document.getElementsByTagName('rect');
//rectangle[rectangle.length-1] - draws outer rect.

//to expand plotRegion rectangle to show labels:
var rect = document.getElementsByTagName('rect')[2];
rect.setAttribute('width', rect.getAttribute('width')*1.5);
rect.setAttribute('height', rect.getAttribute('height')*1.5);
rect.setAttribute('x', rect.getAttribute('x') -20);
rect.setAttribute('y', rect.getAttribute('y')-20);


//PARSING DATA:
intervals  = JSON.parse(intervals)
counts  = JSON.parse(counts)
prop = JSON.parse(prop);
boxData = JSON.parse(boxData);

count = counts.length;

//identify polygon bars:
p = document.getElementsByTagName('polygon')[2]; // to skip the first two polygons that correspond to the boxplot at the bottom.
var id = p.getAttribute('id');
var Grob = id.substring(0, id.lastIndexOf('.'));
var panel = document.getElementsByTagName('g')[1];

//creating group labels:
gLabel = function(Grob, i) {
var panel = document.getElementById(Grob);
var gEl = document.createElementNS("http://www.w3.org/2000/svg", "g");
    gEl.setAttributeNS(null, 'id', 'gLabel' + i);
    gEl.setAttributeNS(null, 'visibility', 'hidden');
    panel.appendChild(gEl);
  }

//function to create rectangles for labels:
gRect = function(i) {
    var gRect = document.createElementNS("http://www.w3.org/2000/svg", "rect");
        gRect.setAttributeNS(null, 'visibility', 'inherit');
        gRect.setAttributeNS(null, 'id', 'gRect' + i);
        gRect.setAttributeNS(null,'fill', 'white');
        gRect.setAttributeNS(null,'fill-opacity', '0.8');
        gRect.setAttributeNS(null,'rx', '5');
        gRect.setAttributeNS(null, 'ry', '5');
        gRect.setAttributeNS(null, 'stroke', 'none');
    gLabel = document.getElementById('gLabel' + i);
    gLabel.appendChild(gRect);
  };


for (i = 1; i <= count; i++) {
  gLabel(Grob, i);
}

for (i = 1; i <= count; i++) {
  gRect(i);
}

label = function(id, textinput, i, tf) {
//attributes for the text SVG element
  var label = document.createElementNS("http://www.w3.org/2000/svg", "text");
    label.setAttributeNS(null, 'x', '0');
    label.setAttributeNS(null, 'y', '0');
    label.setAttributeNS(null, 'font-size', "12");
    label.setAttributeNS(null, 'fill', 'black');
    label.setAttributeNS(null, 'stroke', 'none');
    label.setAttributeNS(null, 'fill-opacity', '1');
    label.setAttributeNS(null, 'transform', 'translate('+ ((x+sx)/2) + ', ' + (y + tf) +') scale(1, -1)');
    label.setAttributeNS(null, 'text-anchor', 'middle');
    label.setAttributeNS(null, 'visibility', 'inherit'); //hidden
    label.setAttributeNS(null, 'id', id + i);

// Creating the text label element:
  var text = textinput;
  var textNode = document.createTextNode(text);

    label.appendChild(textNode);
    gLabel = document.getElementById('gLabel' + i);
    gLabel.appendChild(label);
};

//creating tspan labels - for customizing text in bold:
tLabel = function(id, textinput, i, tf) { // tf is how much to move the text higher than y in px
  var tLabel = document.createElementNS("http://www.w3.org/2000/svg", "tspan");
  tLabel.setAttributeNS(null, 'visibility', 'inherit');
  tLabel.setAttributeNS(null, 'id', id + i);
  tLabel.setAttributeNS(null, 'transform', 'translate('+ ((x+sx)/2) + ', ' + (y + tf) +') scale(1, -1)');
  tLabel.setAttributeNS(null, 'font-weight', 'bold');

  tLabel.innerHTML = textinput;
  lab.appendChild(tLabel);

}

// Creating all the labels:
for (i  = 1; i <= count; i++) {
  var bar = document.getElementById(Grob + '.' + i);
  bar.setAttribute('stroke', 'white');
  bar.setAttribute('stroke-width', '0.25');
  bar.setAttribute("class", i);

  var coords = bar.getAttribute('points');
  var small = coords.split(" ")[1];
  var sx = Number(small.split(",")[0]);
  var coordsxy = coords.split(" ")[2];
  var x = Number(coordsxy.split(",")[0]); //co-ordinates based upon SVG elements.
  var y = Number(coordsxy.split(",")[1]);
  label('label', 'Class range: ', i, 45); // deal with t-span later.
  lab = document.getElementById('label' + i);
  tLabel('tLabel', intervals[i-1].toFixed(2) + '-' + intervals[i].toFixed(2), i, 45);

  label( 'countLabel','N = ' , i, 30);
  lab = document.getElementById('countLabel' + i);
  tLabel('tcountLabel',counts[i-1], i, 30);

  label('propLabel', (prop[i-1]*100).toFixed(2) + "%", i, 15);
  var propLabel = document.getElementById('propLabel' + i);
    propLabel.setAttribute('font-weight', 'bold');

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
polygonId = polygonBox[0].id; // this differs from dotplots - the boxplot appears to be drawn first...
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
  boxLabel.setAttributeNS(null, 'x', '0');
  boxLabel.setAttributeNS(null, 'y', '0');
  boxLabel.setAttributeNS(null, 'font-size', "10");
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
    line = document.getElementById(lastLine + '.' +  p); // p will either be 1 or 2 -> 1 = minLine, 2 = maxLine
    line.setAttribute('class', 'box');
    boxPoints = line.getAttribute('points').split(" ")[r].split(",");
  } else {
    box = document.getElementsByClassName('box')[p]; // boxplot split into two boxes - lowerbox (p = 0) and upperbox (p = 1)
    boxPoints = box.getAttribute('points').split(" ")[r].split(",");
  }
  x = boxPoints[0];
  y = boxPoints[1];
  text = textinput + ": " + boxData[q].quantiles; // this is associated with the boxData imported from R.
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
  boxData = document.getElementsByClassName('boxData');
  for (i =0; i < boxData.length; i++) {
    boxData[i].setAttribute('visibility', 'visible');
}
};


/* ------------------------------------------------------------
      Histogram Interactivity - hovers, clicks on bars and in
      relation to table
---------------------------------------------------------------*/
//setting mouse events:
for (i = 1; i <= count; i++) {
 bar = document.getElementById(Grob + '.' + i);
 bar.setAttribute('onmouseover', 'light('+ i + ')');
 bar.setAttribute('onmouseout', 'normal(' + i +')');
bar.setAttribute('onclick', 'fade(' + i +')');
 }

 light = function(i) {
   bar = document.getElementById(Grob + '.' + i);
   bar.setAttribute('fill-opacity', '0.5');
   gLabel = document.getElementById('gLabel' + i);
   gLabel.setAttribute('visibility', 'visible');
 };

 normal = function(i) {
   bar = document.getElementById(Grob + '.' + i);
   bar.setAttribute('fill-opacity', '1');
   gLabel = document.getElementById('gLabel' + i);
   gLabel.setAttribute('visibility', 'hidden');
 };


 fade = function(i) {
   for (j = 1; j <= count; j ++) {

     bar = document.getElementById(Grob + '.' + j);
        l = bar.getAttribute('fill');
       lp = l.substring(l.lastIndexOf("("), l.lastIndexOf(")"));

     gLabel = document.getElementById('gLabel' + j);
     //tablerow:
     dataRow = document.getElementById('tr' + j);
     totalRow = document.getElementById('totalRow');
     totalRow.style.display = "none";

     if (i == j) {
       bar.setAttribute('opacity', '1');
       gLabel.setAttribute('visibility', 'visible');
       //table:
       dataRow.style.display = "table-row";
       dataRow.style.backgroundColor = "rgba" + lp + ", 0.3)";
       dataRow.style.fontWeight = "bold";

     } else {
       bar.setAttribute('opacity', '0.3');
       gLabel.setAttribute('visibility', 'hidden');
       dataRow.style.display = "none";
     }
 }
 boxData = document.getElementsByClassName('boxData');
  for (k = 0; k < boxData.length; k++) {
    boxData[k].setAttribute('visibility', 'hidden');
  }
 };


 //Reset Button:
   reset = function() {
     for (i = 1; i <= count; i++) {
       bar = document.getElementById(Grob + "." + i);
       bar.setAttribute('opacity', '1');
       bar.setAttribute('visibility', 'visible');
       gLabel = document.getElementById('gLabel' + i);
       gLabel.setAttribute('visibility', 'hidden');
       dataRow = document.getElementById('tr' + i);
       dataRow.style.display = "table-row";
       dataRow.style.backgroundColor = "white";
       dataRow.style.opacity = "1";
       dataRow.style.fontWeight = "normal";
       totalRow = document.getElementById('totalRow');
       totalRow.style.display = "table-row";
   }
   boxData = document.getElementsByClassName('boxData');
    for (k = 0; k < boxData.length; k++) {
      boxData[k].setAttribute('visibility', 'hidden');
    }
    table.style.display = "none";
    viewTable.innerHTML = "View Table";
    t = true;
 };

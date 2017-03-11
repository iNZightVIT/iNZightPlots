/* JavaScript code for dot plots and scatterplots (inclusive of iNZightMaps):
Code is split into 3 sections: table properties,
                                interactions,
                                selectionCanvas (for multi-selection of points)

/* -----------------------------------------------------
                Table properties:
Code to assign classes, ids to table cells and rows to
link up to interactions on the plot.
Creation of HTML form/select to allow user to select
the variables to display in labels and in the table.
-------------------------------------------------------- */
var table = document.getElementById('table');

//no. of rows in table
var nrow = document.getElementById('table').rows.length;

//no. of columns in table
var ncol = document.getElementsByTagName('th').length;

var td = document.getElementsByTagName('td');
cellNo = td.length;

for (i = 1; i <= cellNo; i ++) {
  td[i-1].setAttribute('id', i);
};

for (j = 1; j <= ncol; j++) {
  th = document.getElementsByTagName('th');
  th[j-1].setAttribute('class', j-1);

  for (i=1; i <= cellNo; i++) {
  var td = document.getElementsByTagName('td');
    if (i%ncol == j) {
      td[i].setAttribute('class', j);
    }
  }
};

//no. of rows in table
nrow = table.rows.length;
for (i = 1; i < nrow; i ++) {
  var tr = document.getElementsByTagName('tr');
  tr[i].setAttribute('id', 'tr' + i);
};

//  Select option for interactivity: to select variables accordingly
var form = document.createElement('form');
form.setAttribute('class', 'form-inline');
form.setAttribute('id', 'form');
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
      opt.classList.add('select');
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
    table.classList.remove('hidden');
    t = false;
  } else {
    viewTable.innerHTML = "View Table";
    table.classList.add('hidden');
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

var svg = document.getElementsByTagName('svg')[0];
svg.setAttribute('preserveAspectRatio', 'xMinYMin meet');

//set container with no style padding:
var svgContainer = document.getElementById('svg-container');
svgContainer.classList.add('contained');

//to expand plotRegion rectangle to show labels:
var rect = document.getElementsByTagName('rect')[0];
rect.setAttribute('class', 'rect');

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
  var panel = document.getElementsByTagName('g')[0];

  //BOX PLOT LABELS:
  /* The box plot is made up of 2 lines (line that extends to the minimum, and
  the other extending to the maximum)
  and two 'polygon' rectangles that make up the box (a lower box that draws up
  to the median, while the upper draws from the median to the upper quartile
  value).
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

  //Min and Max - obtaining the ends of of the boxplot (lines): these are
  //identified as the last two lines in the 'polyline' group.
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
  var boxLabel = document.createElementNS("http://www.w3.org/2000/svg", "text");
    boxLabel.setAttribute('class', 'label boxData hidden');
    boxLabel.setAttributeNS(null, 'transform', 'translate(' + Number(x) + ',' + (Number(y) + 10) + ') scale(1, -1)');
    boxLabel.setAttributeNS(null, 'id', textinput);

    var textNode = document.createTextNode(textinput);
    boxLabel.appendChild(textNode);
    panel.appendChild(boxLabel);
  };

  boxLabelSet = function(p, r, q, textinput) {
    if (textinput == "Min" ||  textinput == "Max") {
      var line = document.getElementById(lastLine + '.' +  p);
      // p will either be 1 or 2 -> 1 = minLine, 2 = maxLine
      line.setAttribute('class', 'box');
      var boxPoints = line.getAttribute('points').split(" ")[r].split(",");
    } else {
      var box = document.getElementsByClassName('box')[p];
      // boxplot split into two boxes - lowerbox (p = 0) and upperbox (p = 1)
      var boxPoints = box.getAttribute('points').split(" ")[r].split(",");
    }
    x = boxPoints[0];
    y = boxPoints[1];

    if (textinput == "Median") {
      // move median label below the box plot
     y = boxPoints[1] - 25;
    }

    text = textinput + ": " + boxData[q].quantiles;
    // this is associated with the boxData imported from R. q = 0 (LQ), 1 (UQ), 2 (Median), 3 (Min), 4 (Max)
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
  box[i].setAttribute('onmouseover', 'fillBox()');
  box[i].setAttribute('onmouseout', 'normalBox()');
  box[i].setAttribute('onclick', 'showBox()');
  }

  fillBox = function() {
    for (i = 0; i < box.length; i++) {
    box[i].classList.add('fillBox');
  }
  };

  normalBox = function() {
    for (i = 0; i < box.length; i++) {
      box[i].classList.remove('fillBox');
  }
  };

  showBox = function() {
    var boxData = document.getElementsByClassName('boxData');
    for (i =0; i < boxData.length; i++) {
      boxData[i].classList.remove('hidden');
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
    gEl.setAttributeNS(null, 'class', 'gLabel invisible');
    panel.appendChild(gEl);
  };

//function to create rectangles for labels:
  gRect = function(i) {
    var gRect = document.createElementNS("http://www.w3.org/2000/svg", "rect");
        gRect.setAttributeNS(null, 'id', 'gRect' + i);
        gRect.setAttributeNS(null, 'class', 'gRect');
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
label = function(id, textinput, i, j) {

var label = document.createElementNS("http://www.w3.org/2000/svg", "text");
  label.setAttributeNS(null, 'class', 'label');
  label.setAttributeNS(null, 'transform', 'translate(' + Number(x) + ',' + (Number(y) + (varNo-j)*15) + ') scale(1, -1)'); //hardcoded!
  label.setAttributeNS(null, 'id', id + i);

  var textNode = document.createTextNode(textinput);

    label.appendChild(textNode);
    var gLabel = document.getElementById('gLabel' + i);
    gLabel.appendChild(label);

};

//creating tspan labels - for customizing text in bold:
tLabel = function(id, textinput, i) {
  var tLabel = document.createElementNS("http://www.w3.org/2000/svg", "tspan");
  tLabel.setAttributeNS(null, 'class', 'tLabel');
  tLabel.setAttributeNS(null, 'id', id + i);

  var textNode = document.createTextNode(textinput);
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
  label('labelNo', textNo, i, 0);
  text = [];
  text[j] = names[j] + ": ";
  label('label' + '.' + (j+1) + '.' , text[j], i, j+1);

  var lab = document.getElementById('label' + '.' + (j+1) + '.' + i);
  tLabel('tLabel', tableData[i-1][names[j]], i);
  lab.classList.add((j+1));

// Attach and draw rectangles to labels according to the size of the gLabel (with all labels attached)
  var gLabel = document.getElementById('gLabel' + i);
  var rectParam = gLabel.getBBox();
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
  point.setAttribute('onmouseover', 'light(' + i + ')');
  point.setAttribute('onmouseout', 'normal(' + i + ')');
  point.setAttribute('onclick', 'info(' + i + ')');
};

//Hover on:
light = function(i) {
  var point = document.getElementById(Grob + "." + i);
  point.classList.add('showPoint');
  var gLabel = document.getElementById('gLabel' + i);
  gLabel.classList.remove('invisible');

};

//Hover out:
normal = function(i) {
  var point = document.getElementById(Grob + "." + i);
  point.classList.remove('showPoint');

  var gLabel =document.getElementById('gLabel' + i);
  gLabel.classList.add('invisible');

};

//On click:
info = function(i) {
  for (j = 1; j <= count; j++) {
    var point = document.getElementById(Grob + "." + j);
    var gLabel = document.getElementById('gLabel' + j);

    var l = point.getAttribute('stroke');
    var lp = l.substring(l.lastIndexOf("("), l.lastIndexOf(")"));

    var dataRow = document.getElementById('tr' + j);

    if (i == j) {
      gLabel.classList.remove('invisible');

      point.setAttribute('class', 'point selected');

      dataRow.style.backgroundColor = "rgba" + lp + ", 0.25)";
      dataRow.classList.remove('hidden');
      dataRow.classList.add('rowSelect');

    } else {
      gLabel.classList.add('invisible');
      point.setAttribute('class', 'point none');

      dataRow.classList.remove('rowSelect');
      dataRow.classList.add('hidden');
      dataRow.style.backgroundColor = "white";

    }
  }
  if (boxData != undefined) {
 boxData = document.getElementsByClassName('boxData');
  for (i =0; i < boxData.length; i++) {
    boxData[i].classList.add('hidden');
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
  key.setAttribute('onmouseover', 'show(' + i +')');
  key.setAttribute('onmouseout', 'out(' + i + ')');
  key.setAttribute('onclick', 'subset(' + i + ')');
  var keyText = document.getElementById(text[i+3].id);
  if (Grob == "DOTPOINTS.1") { // for dot plots - legend text differs
    var keyText = document.getElementById(text[i+2].id);
  }
  keyText.setAttribute('onmouseover', 'show(' + i +')');
  keyText.setAttribute('onmouseout', 'out(' + i +')');
  keyText.setAttribute('onclick', 'subset(' + i + ')');
};

// hover on a legend group:
show = function(i) {
  var keyText = document.getElementById(text[i+3].id);
  if (Grob == "DOTPOINTS.1") {
    var keyText = document.getElementById(text[i+2].id);
  }
  var key = document.getElementById(keys[i-1].id);
  keyText.setAttribute('fill', key.getAttribute('fill'));
  keyText.setAttribute('class', 'show');
  key.setAttribute('class', 'show');

};

//hover out:
out = function(i) {
  var keyText = document.getElementById(text[i+3].id);
  if (Grob == "DOTPOINTS.1") {
    var keyText = document.getElementById(text[i+2].id);
  }
  var key = document.getElementById(keys[i-1].id);
  keyText.setAttribute('class', 'out keyText');
  key.setAttribute('class', 'out');

};

//on click, subsetting occurs:
subset = function(i) {
  for (j = 1; j <= count; j++) {
    var point = document.getElementById(Grob + '.' + j);
    var key = document.getElementById(keys[i-1].id);
    var gLabel = document.getElementById('gLabel' + j);
    var dataRow = document.getElementById('tr' + j);

if (key.getAttribute('fill') == point.getAttribute('stroke')) {
  point.setAttribute('class', 'point selected');

  dataRow.classList.remove('hidden');
  dataRow.classList.add('rowSelect');

  dataRow.style.backgroundColor = "white";
} else {
  point.setAttribute('class', 'point none');
  dataRow.classList.add('hidden');
  dataRow.style.backgroundColor = "white";

}
}
}
};

/* Link to interactive table + labels - "Variables to display" select/option box:
- Requires revision due to browser incompatibility. */

selected = function() {
sOpt = selectVar.selectedOptions;
// this does not work on IE, and not fully supported. May need to replace this.
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
Code to select over a group of points via mouse drag.
//Need to test on IE.
----------------------------------------------------------------- */

//obtaining svg region:
var svg = document.getElementsByTagName('svg')[0];
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

var zoomBox = {};

MouseDown = function(evt) {
    zoomBox["startX"] = evt.pageX - 20;
    zoomBox["startY"] = evt.pageY - 20;
    zoomBox["isDrawing"] = true;
   selectRect.setAttribute('points',  zoomBox["startX"] + ',' + zoomBox["startY"]);
};

MouseUp = function(evt) {
  svg.style.cursor = "default";
      zoomBox["endX"] = evt.pageX - 20;
      zoomBox["endY"] = evt.pageY - 20;
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

        for (i =1; i <= count; i++) {
        var point = document.getElementById(Grob + '.' + i);
        var gLabel = document.getElementById('gLabel' + i);
        var dataRow = document.getElementById('tr' + i);

          var x = point.x.baseVal.value;
          var y = point.y.baseVal.value;

          if (point.getAttribute('visibility') == 'hidden') {
            // those that are hidden, remain hidden
              point.classList.add('hidden');
              gLabel.classList.add('invisible');
            } else {
              //points that lie within the  boundary box drawn:
          if((x1 <= x && x <= x2) && (y1 <= y && y <= y2)) {
            point.setAttribute('class', ' point selected');
            l = point.getAttribute('stroke');
            lp = l.substring(l.lastIndexOf("("), l.lastIndexOf(")"));

            gLabel.classList.remove('invisible');
            dataRow.classList.remove('hidden');
            dataRow.classList.add('rowSelect');
            dataRow.style.backgroundColor = "rgba" + lp + ", 0.25)";

           } else {
             point.setAttribute('class', 'point none');
             gLabel.classList.add('invisible');

             dataRow.classList.add('hidden');
             dataRow.classList.remove('rowSelect');
             dataRow.style.backgroundColor = "white";
           }
         }
        }
    }
};


/* -------------------------------------------------
      Reset button - attempts to return to original state
-------------------------------------------------- */
//Reset Button:
  reset = function() {
    for (i = 1; i <= count; i++) {
    var point = document.getElementById(Grob + "." + i);
      point.setAttribute('class', 'point');

    var gLabel = document.getElementById('gLabel' + i);
    gLabel.classList.add('invisible');

    var dataRow = document.getElementById('tr' + i);
      dataRow.classList.remove('hidden');
      dataRow.classList.remove('rowSelect');

      dataRow.style.backgroundColor = "white";
    }
    // reset selection rectangle
    var selectRect = document.getElementById('selectRect');
    if (selectRect != undefined) {
      selectRect.setAttribute('points', '0,0');
  }

for (i =1; i <= ncol-1; i++) {
    column = table.getElementsByClassName(i);
    for (j = 1; j <=column.length; j++) {
    column[j-1].style.display = "table-cell";
  }
}
};

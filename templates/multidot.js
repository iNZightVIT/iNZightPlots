/* NEW TRIAL FOR MULTI- DOTPLOTS: (1 cat, 1 cont variable) */

var svg = document.getElementsByTagName('svg')[0];

//restrict svg-container for drag box to align
var container = document.getElementById('svg-container');
container.classList.add('contained');

var levelNo = levels.length;
var Grob = "DOTPOINTS";


// TABLE ATTRIBUTES: labelling each row!
var table = document.getElementById('table')
    nrow = table.rows.length, //no. of rows in table
    ncol = document.getElementsByTagName('th').length, //no. of columns in table
    td = document.getElementsByTagName('td'),
    tr = document.getElementsByTagName('tr'),
    cellNo = td.length,
    totalCount = tr.length-1;

//labelling:
for (j = 1; j <= ncol; j++) {
  th = document.getElementsByTagName('th');
  th[j-1].setAttribute('class', j);
};

  for (i = 1; i <= cellNo; i ++) {
    td[i-1].setAttribute('id', i);
    td[i-1].setAttribute('class', i%ncol);
    if (i%ncol == 0) {
      td[i-1].setAttribute('class', ncol);
    }
  };

//labelling table rows with id numbers:
//separate tr by groups:
var trGroups = [];
for (i =0; i <= countsTab.length; i++) {
  //divide into groups
  trGroups[i] = Array.prototype.slice.call(tr, countsTab[i]+1, countsTab[i+1]+1);
  //then label for each group
  for(j = 1; j <= trGroups[i].length; j++){
    trGroups[i][j-1].id = 'tr' + '.' + (i+1) + '.' + j;
  }
};

//Viewtable button:
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
for (i = 0; i<=ncol; i++){
    var opt = document.createElement('option');
    if (i == 0) {
      opt.value = 0;
      opt.classList.add('select');
      opt.innerHTML = "Display all";
      opt.selected = "selected";
    } else {
    opt.value = i;
    opt.innerHTML = th[i-1].innerHTML;
  }
  selectVar.appendChild(opt);
};

selected = function() {
  table = document.getElementsByTagName('table')[0];
sOpt = selectVar.selectedOptions;
// this does not work on IE, and not fully supported. May need to replace this.
s = [];
for (i =0; i < sOpt.length; i++) {
  s.push(sOpt[i].value);
};

for (i =1; i <= ncol; i++) {
  var column = table.getElementsByClassName(i);
  var labels = svg.getElementsByClassName(i);

  for(j = 1; j <= column.length; j++) {
    if (s.indexOf('0') >= 0) {
      column[j-1].style.display = "table-cell";
      if (j <= labels.length) {
        labels[j-1].style.display = "inherit";
      //  detachRectLabel(j);
      //  gRect(j);
      //  drawRectLabel(j);
      }
    } else {
    column[j-1].style.display = "none";
    if (j <= labels.length) {
    //addClass(labels[j-1], 'hidden');
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
     //redraw rectangles according to new label size:
     //detachRectLabel(j);
     //gRect(j);
     //drawRectLabel(j);
  }
}
}
}
};


// creating labels:
gLabel = function(i, j) {
var panel = document.getElementsByTagName('g')[0];
var gEl = document.createElementNS("http://www.w3.org/2000/svg", "g");
    gEl.setAttributeNS(null, 'id', 'gLabel' + '.' + i + '.' + j);
    gEl.setAttributeNS(null, 'class', 'gLabel invisible'); //invisible'
    panel.appendChild(gEl);
  };

//function to create rectangles for labels:
  gRect = function(i, j) {
    var gRect = document.createElementNS("http://www.w3.org/2000/svg", "rect");
        gRect.setAttributeNS(null, 'id', 'gRect' + '.' + i + '.' + j);
        gRect.setAttributeNS(null, 'class', 'gRect');
    gLabel = document.getElementById('gLabel' + '.' + i + '.' + j);
    gLabel.appendChild(gRect);
  };

//making rectangles and g elements for each point:
for (i = 1; i <= levelNo; i++) {
  var plot = document.getElementById(Grob + '.' + i);
  var count = plot.childElementCount;
for (j = 1; j <= count; j++) {
  gLabel(i,j);
}
};

for (i = 1; i <= levelNo; i++) {
  var plot = document.getElementById(Grob + '.' + i);
  var count = plot.childElementCount;
for (j = 1; j <= count; j++) {
  gRect(i,j);
}
};

//text labels:
label = function(id, textinput, i, j, k) {

var label = document.createElementNS("http://www.w3.org/2000/svg", "text");
  label.setAttributeNS(null, 'class', 'label' + ' ' + id);
  label.setAttributeNS(null, 'transform', 'translate(' + Number(x) + ',' + (Number(y) + (k*15)) + ') scale(1, -1)'); //hardcoded!
  label.setAttributeNS(null, 'id', id +  '.' + i + '.' + j);

  var textNode = document.createTextNode(textinput);

    label.appendChild(textNode);
    var gLabel = document.getElementById('gLabel' + '.' + i + '.' + j);
    gLabel.appendChild(label);

};


function tLabel(id, textinput, i, j, lab) {
    var tLabel = document.createElementNS("http://www.w3.org/2000/svg", "tspan");
    tLabel.setAttributeNS(null, 'class', 'tLabel' + ' ' + id);
    tLabel.setAttributeNS(null, 'id', id + '.' + i + '.' + j);

    var textNode = document.createTextNode(textinput);
    tLabel.appendChild(textNode);
    lab.appendChild(tLabel);

  }

//Create number and value labels:  -for one set only
for (i =1; i <= levelNo; i++) {
  var plot = document.getElementById(Grob + '.' + i);
  var count = plot.childElementCount;
for (j  = 1; j <= count; j++) {
  var point = document.getElementById(Grob + '.' + i + '.' + j);
  var x = point.getAttribute('x');
  var y = point.getAttribute('y');

  //create labels:
  label('yLabel', names.y + ": " , i, j, 2); //levNames[i-1]
  var lab = document.getElementById('yLabel.' + i + '.' + j);
  tLabel('tLabel.yLabel', levNames[i-1], i, j, lab);
  //hardcoded atm - need to find a way if more variables are to be added.
  label('xLabel', names.x + ": ", i, j, 1);
  var lab = document.getElementById('xLabel.' + i + '.' + j);
  tLabel('tLabel.xLabel', levels[i-1][j-1], i, j, lab);

// Attach and draw rectangles to labels according to the size of the gLabel (with all labels attached)
  var gLabel = document.getElementById('gLabel' + '.' + i + '.' + j);
  var rectParam = gLabel.getBBox();
  var gRect = document.getElementById('gRect' + '.' + i + '.' + j);
  gRect.setAttribute('x', rectParam.x-1);
  gRect.setAttribute('y', rectParam.y-2);
  gRect.setAttribute('width', rectParam.width+2);
  gRect.setAttribute('height', rectParam.height+2);

}
};

// interactivity:
for (i =1; i <= levelNo; i++) {
var plot = document.getElementById(Grob + '.' + i);
var count = plot.childElementCount;
  for (j = 1; j <= count; j++) {
    point = document.getElementById(Grob + '.' +  i + '.' + j);
    point.setAttribute('onmouseover', 'light(' + i + ',' + j +')');
    point.setAttribute('onmouseout', 'normal(' + i + ',' + j + ')');
    point.setAttribute('onclick', 'info(' + i + ',' + j + ')');
  }
};

light = function(i,j) {
  var point = document.getElementById(Grob + '.' + i + '.' + j);
  var gLabel = document.getElementById('gLabel' + '.' + i + '.' + j);
  point.classList.add('showPoint');
  gLabel.classList.remove('invisible');
};

normal = function(i,j) {
  var point = document.getElementById(Grob + '.' + i + '.' + j);
  var gLabel = document.getElementById('gLabel' + '.' + i + '.' + j);
  point.classList.remove('showPoint');
  gLabel.classList.add('invisible');
};


//TO DO: investigate a way to eliminate/avoid use of several for loops
info = function(i,j) {
  for (k = 1; k <= levelNo; k++) {
    var plot = document.getElementById(Grob + '.' + k);
    var count = plot.childElementCount;
    for (l = 1; l <= count; l++) {
      var point = document.getElementById(Grob + '.' + k + '.' + l);
      var gLabel = document.getElementById('gLabel.' + k + '.' + l);
      var dataRow = document.getElementById('tr.' + k + '.' + l);

      //match color to point:
      var pointColor = point.getAttribute('stroke'),
           rgbCol = pointColor.substring(pointColor.lastIndexOf("("), pointColor.lastIndexOf(")"));


      if (k == i && l == j) {
        point.setAttribute('class', 'point selected');

        //add on links to table:
        dataRow.style.backgroundColor = "rgba" + rgbCol + ", 0.25)";
        dataRow.classList.add('rowSelect');
        dataRow.classList.remove('hidden');

      } else {
        point.setAttribute('class', 'point none');

        // add on links to table:
        dataRow.style.backgroundColor = "white";
        dataRow.classList.add('hidden');
        dataRow.classList.remove('rowSelect');

      }
    }
  }
}

// BOX PLOTS:
//Obtaining the 'polygon' boxes associated with the boxplots:
var polygonBoxes = document.getElementsByTagName('polygon');
var polygonBox = [];
for (i = 0; i < levelNo; i++) { //separate boxes for each plot
  polygonBox[i] = Array.prototype.slice.call(polygonBoxes, i*2, i*2+2);
  polygonBox = polygonBox[i];
  for (j=0; j < 2; j++) {
    polygonBox[j].setAttribute('class', 'box' + ' ' + (i+1));
  }
};

// lines  associated with boxplots:
var polyLines = document.getElementsByTagName('polyline');

//Separate lines and tick labels
for (i = 1; i <= polyLines.length; i++) {
  if (polyLines[i-1].id.indexOf('GRID') >= 0) {
    polyLines[i-1].setAttribute("class", "line");
  }
};

//lines associated with box plots are plotted after the grid lines.
var lines = document.getElementsByClassName("line");
//Separate array from grid lines to box lines:
var boxLines = Array.prototype.slice.call(lines, lines.length-2*levelNo, lines.length);

//Separate between levels:
var minMax = [];
for (i = 0; i< levelNo; i++) {
  minMax[i] = Array.prototype.slice.call(boxLines, i*2, i*2+2);
  lastLine = minMax[i];
  for(j = 0; j < 2; j++) {
    lastLine[j].setAttribute('id', 'lastLine.' + (i+1) + '.' + (j+1));
  }
}

//count = document.getElementById(Grob).childElementCount;
var panel = document.getElementsByTagName('g')[0];

//functions to create boxLabels:
boxLabel = function(i, textinput) {
var boxLabel = document.createElementNS("http://www.w3.org/2000/svg", "text");
  boxLabel.setAttribute('class', 'label boxData ' + i + ' hidden'); //hidden
  boxLabel.setAttributeNS(null, 'transform', 'translate(' + Number(x) + ',' + (Number(y) + 2) + ') scale(1, -1)');
  boxLabel.setAttributeNS(null, 'id', textinput);

  var textNode = document.createTextNode(textinput);
  boxLabel.appendChild(textNode);
  panel.appendChild(boxLabel);
};


boxLabelSet = function(i, p, r, q, textinput) {
  if (textinput == "Min" ||  textinput == "Max") {
    var line = document.getElementById('lastLine.' + i  + '.' + p); //i is levelNo.
    // p will either be 1 or 2 -> 1 = minLine, 2 = maxLine
    line.setAttribute('class', 'box' + ' ' + i);
    var boxPoints = line.getAttribute('points').split(" ")[r].split(",");
  } else {
    var box = document.getElementsByClassName('box' + ' ' + i)[p];
    // boxplot split into two boxes - lowerbox (p = 0) and upperbox (p = 1)
    var boxPoints = box.getAttribute('points').split(" ")[r].split(",");
  }
  x = boxPoints[0];
  y = boxPoints[1];

  if (textinput == "Median") {
    // move median label below the box plot
   y = boxPoints[1] - 15;
  }

  text = textinput + ": " + boxData[i-1][q].quantiles;
  // this is associated with the boxData imported from R. q = 0 (LQ), 1 (UQ), 2 (Median), 3 (Min), 4 (Max)
  boxLabel(i, text);
};

for (i =1; i <= levelNo; i++) {
  boxLabelSet(i, 0, 1, 0, 'LQ');
  boxLabelSet(i, 1, 2, 2, 'UQ');
  boxLabelSet(i, 1, 0, 1, 'Median');
  boxLabelSet(i, 1, 0, 3, 'Min');
  boxLabelSet(i, 2, 1, 4, 'Max');
}


//Box Plot interactions:
for (j = 1; j <= levelNo; j++) {
var box = document.getElementsByClassName('box' + ' ' + j);

//setting interactions and colors for box plot:
for (i = 0; i < box.length; i++) {
box[i].setAttribute('onmouseover', 'fillBox(' + j + ')');
box[i].setAttribute('onmouseout', 'normalBox(' + j + ')');
box[i].setAttribute('onclick', 'showBox(' + j + ')');
}
};

fillBox = function(j) {
  var box = document.getElementsByClassName('box' + ' ' + j);
  for (i = 0; i < box.length; i++) {
  box[i].classList.add('fillBox');
}
};

normalBox = function(j) {
  var box = document.getElementsByClassName('box' + ' ' + j);
  for (i = 0; i < box.length; i++) {
    box[i].classList.remove('fillBox');
    }
  };

showBox = function(j) {
    var boxData = document.getElementsByClassName('boxData ' + j);
  for (i =0; i < boxData.length; i++) {
    boxData[i].classList.remove('hidden');
}
}

//Add dragbox selection:
svg.setAttribute('draggable', 'false');

/*var rect = document.getElementsByTagName('rect'),
    width = svg.width.baseVal.value,
    height = svg.height.baseVal.value; */

//putting selection rectangle in a group element:
var g = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  g.setAttributeNS(null, 'id', 'selectionBox');
  var panel = document.getElementsByTagName('rect')[1].parentNode;
  panel.appendChild(g);
  var selectRect = document.createElementNS('http://www.w3.org/2000/svg', 'polygon');
  selectRect.setAttributeNS(null, 'id', 'selectRect');
  selectRect.setAttributeNS(null, 'class', 'selectRect');
  g.appendChild(selectRect);

  //createSelectionBox(Grob);

var evt = window.event;

//Attach mouse events:
svg.setAttribute('onmouseup', 'MouseUp(evt)');
svg.setAttribute('onmousemove', 'MouseDrag(evt)');
svg.setAttribute('onmousedown', 'MouseDown(evt)');



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
                                          
        var viewTable = document.getElementById('viewTable');
        viewTable.innerHTML = "Hide Table";
        table.classList.remove('hidden');
        t = false;

        for (i = 1; i <= levelNo; i++) {
            var plot = document.getElementById(Grob + '.' + i);
            var count = plot.childElementCount;

        for (j = 1; j <= count; j++) {
            var point = document.getElementById(Grob + '.' + i + '.' + j);
            //var gLabel = document.getElementById('gLabel.' + i + '.' + j);
            //var gRect = document.getElementById('gRect.' + i + '.' + j);
            var dataRow = document.getElementById('tr.' + i + '.' + j);

          var x = point.x.baseVal.value;
          var y = point.y.baseVal.value;

              //points that lie within the  boundary box drawn:
          if((x1 <= x && x <= x2) && (y1 <= y && y <= y2)) {
            point.setAttribute('class', ' point selected');
            pointColor = point.getAttribute('stroke');
            rgbCol = pointColor.substring(pointColor.lastIndexOf("("), pointColor.lastIndexOf(")"));

            //gLabel.classList.remove('invisible');
            //gRect.classList.add('hidden');

            //returnRowSelection(lp, dataRow);
            dataRow.classList.remove('hidden');
            dataRow.classList.add('rowSelect');
            dataRow.style.backgroundColor = "rgba" + rgbCol + ", 0.25)";

           } else {
             point.setAttribute('class', 'point none');
             gRect.classList.remove('hidden');
             gLabel.classList.add('invisible');

              //omitRowSelection(dataRow);
             dataRow.classList.add('hidden');
             dataRow.classList.remove('rowSelect');
             dataRow.style.backgroundColor = "white";
           }
         }
        }
      }
    };



//RESET BUTTON:
function reset() {
  for (i = 1; i <= levelNo; i++) {
    var plot = document.getElementById(Grob + '.' + i);
    var count = plot.childElementCount;
    for (j = 1; j <= count; j++) {
      var point = document.getElementById(Grob + '.' + i + '.' + j);
      var gLabel = document.getElementById('gLabel.' + i + '.' + j);
      var dataRow = document.getElementById('tr.' + i + '.' + j);

      point.setAttribute('class', 'point');
      gLabel.classList.add('invisible');
      dataRow.style.backgroundColor = "white";
      dataRow.classList.remove('hidden');
      dataRow.classList.remove('rowSelect');

      }

    var boxData = document.getElementsByClassName('boxData ' + i);
    for (k = 0; k < boxData.length; k++) {
      boxData[k].classList.add('hidden');
    }
    }

    var selectRect = document.getElementById('selectRect');
    if (selectRect != undefined) {
      selectRect.setAttribute('points', '0,0');
  }

for (i =1; i <= ncol; i++) {
    column = table.getElementsByClassName(i);
    for (j = 1; j <=column.length; j++) {
    column[j-1].style.display = "table-cell";
      }
  }
};


var plotRegion = document.getElementsByTagName('rect')[1];
plotRegion.addEventListener("dblclick", reset, false);

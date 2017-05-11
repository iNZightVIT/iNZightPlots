/* ------------------------------------------
COMMON FUNCTIONS FOR SINGLE PANEL PLOTS:
- A list of common functions that are used to
generate interactivity for single panel plots.
- Created to prevent repetition of code,
and for a better structure + more
human readable (hopefully).
--------------------------------------------*/

/* ------------------------------------------
FUNCTIONS FOR TABLES
--------------------------------------------*/

// insert an x-header:
function insertXHeader() {
  var xrow = table.insertRow(0),
    xhead = xrow.insertCell(0);
xhead.setAttribute('class', 'headings');
xhead.innerHTML = document.getElementsByTagName('tspan')[2].innerHTML;
xhead.colSpan = ncol;
};

// creating a y-header:
function insertYHeader() {
  var yHeading = document.getElementsByTagName('th')[0];
  yHeading.innerHTML = document.getElementsByTagName('tspan')[3].innerHTML;
  yHeading.setAttribute('class',' headings');
}

// extend plotRegion:
function extendPlotRegion(rect) {
  rect.setAttribute('width', rect.getAttribute('width')*2);
  rect.setAttribute('height', rect.getAttribute('height')*2);
  rect.setAttribute('x', rect.getAttribute('x')-20);
  rect.setAttribute('y', rect.getAttribute('y')-20);
}

//create buttons:
function button(name) {
  var button = document.createElement('button');
  button.setAttribute("type", "button");
  button.setAttribute("class","btn btn-primary hidden Button" + name);
  button.innerHTML = "Show " + name;
  button.setAttribute("onclick", "change" + name + "()");
  button.setAttribute('id', 'Button' + name);
  document.getElementById('control').appendChild(button);
};

// add classes and remove classes to elements:
function addClass(id, className) {
  var el = document.getElementById(id);
  if (el == null) {
    return(null);
  } else {
    el.classList.add(className);
  }
}

function removeClass(id, className) {
  var el = document.getElementById(id);
  if (el == null) {
    return(null);
  } else {
      el.classList.remove(className);
  }
}

// Additional forms/selections for dotplots and scatterplots:
function createVariableSelectionForm() {
  //create form
  var form = document.createElement('form');
  form.setAttribute('class', 'form-inline');
  form.setAttribute('id', 'form');
  document.getElementById('control').appendChild(form);

  //create label for form
  var formLabel = document.createElement('label');
  formLabel.setAttribute('for', 'selectVar');
  formLabel.innerHTML = "Variables to display";
  form.appendChild(formLabel);

  //create selection options:
  var selectVar = document.createElement('select');
  selectVar.setAttribute('class', 'form-control');
  selectVar.setAttribute('id', 'selectVar');
  selectVar.setAttribute('onchange', 'selected()');
  selectVar.setAttribute('multiple', 'multiple');
  form.appendChild(selectVar);

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

}

/* ------------------------------------------
FUNCTIONS FOR CREATING LABELS
--------------------------------------------*/
function getGrob(chartType) {
  var polygon = document.getElementsByTagName('polygon');
  if (chartType == 'hist'){
    var p = polygon[2];
  } else if (chartType == 'bar'){
    var p = polygon[polygon.length-1];
  } else {
    var p = polygon[0]; //hexbin, bp-stacked.
  }
  var id = p.getAttribute('id');
  var Grob = id.substring(0, id.lastIndexOf('.'));
  return(Grob);
}

//creating group labels:
function gLabel(i) {
var panel = document.getElementsByTagName('g')[0];
var gEl = document.createElementNS("http://www.w3.org/2000/svg", "g");
    gEl.setAttributeNS(null, 'id', 'gLabel' + i);
    gEl.setAttributeNS(null, 'class', 'gLabel invisible');
    panel.appendChild(gEl);
  }

//function to create rectangles for labels:
function gRect(i) {
    var gRect = document.createElementNS("http://www.w3.org/2000/svg", "rect");
        gRect.setAttributeNS(null, 'id', 'gRect' + i);
        gRect.setAttributeNS(null, 'class', 'gRect');
    gLabel = document.getElementById('gLabel' + i);
    gLabel.appendChild(gRect);
    //insert as the first!
    gLabel.insertBefore(gRect, gLabel.childNodes[0])
  };

function label(id, textinput, i, tf) {
  //attributes for the text SVG element
    var label = document.createElementNS("http://www.w3.org/2000/svg", "text");
      label.setAttributeNS(null, 'transform', 'translate('+ (x) + ', ' + (y + tf) +') scale(1, -1)');
      label.setAttributeNS(null, 'id', id + i);
      label.setAttributeNS(null, 'class', 'label' + ' ' + id);

  // Creating the text label element:
    var textNode = document.createTextNode(textinput);

      label.appendChild(textNode);
      var gLabel = document.getElementById('gLabel' + i);
      gLabel.appendChild(label);
  };

//creating tspan labels - for customizing text in bold:

function tLabel(id, textinput, i, lab) {
    var tLabel = document.createElementNS("http://www.w3.org/2000/svg", "tspan");
    tLabel.setAttributeNS(null, 'class', 'tLabel' + ' ' + id);
    tLabel.setAttributeNS(null, 'id', id + '.' + i);

    var textNode = document.createTextNode(textinput);
    tLabel.appendChild(textNode);
    lab.appendChild(tLabel);

  }

// draw rectangles to labels according to size of label:
function drawRectLabel(i) {
  var gLabel = document.getElementById('gLabel' + i);
  rectParam = gLabel.getBBox();
  var gRect = document.getElementById('gRect' + i);
  gRect.setAttribute('x', rectParam.x-5);
  gRect.setAttribute('y', rectParam.y-2);
  gRect.setAttribute('width', rectParam.width+10);
  gRect.setAttribute('height', rectParam.height+4);
}

//detach rectangle labels:
function detachRectLabel(i) {
  var gRect = document.getElementById('gRect' + i);
  var parent = gRect.parentNode;
  parent.removeChild(gRect);
}

//hide lines in bar plots:
function hideBarLines() {
  var polyline = document.getElementsByTagName('polyline');
  //finding lines that are labeled with "GRID".
  for (i = 0; i < polyline.length; i ++) {
   if (polyline[i].id.indexOf("GRID") >= 0) {
     polyline[i].setAttribute("class", "line");
   }
  }
  var lines = document.getElementsByClassName('line');
  var lastId = lines[lines.length-1].getAttribute('id');
  var lastLine = lastId.substring(0, lastId.lastIndexOf('.'));

  //separating bar lines from axes lines
  for (i = 0; i < lines.length; i++) {
   if(lines[i].id.indexOf(lastLine) >= 0) {
     lines[i].classList.add('hidden');
   } else {
     lines[i].classList.add('visible');
    }
  }
}

/* ------------------------------------------
FUNCTIONS FOR BOXPLOTS
--------------------------------------------*/

// returns the minimum and maximum line id of the box plot:
function getMinMaxLinesId() {
  var polyLines = document.getElementsByTagName('polyline');
  for (i = 1; i <= polyLines.length; i++) {
  if (polyLines[i-1].id.indexOf('GRID') >= 0) {
    polyLines[i-1].setAttribute("class", "line");
  }
  };
  var lines = document.getElementsByClassName("line");
  var lastId = lines[lines.length-1].id;
  var lastLine = lastId.substring(0, lastId.lastIndexOf('.'));
  return(lastLine);
  //obtaining the ends of of the boxplot (lines):
//min line: lastLine + '.' + 0; max line: lastLine + '.' + 1;
}

//obtains the boxes that make up the box plot (2 boxes):
function getBoxes(chartType) {
  //Obtaining the 'polygon' boxes associated with the boxplot:
  // this differs from dotplots - the boxplot appears to be drawn first... (histograms)
  polygonBox = document.getElementsByTagName('polygon');

  if (chartType == "hist") {
    polygonId = polygonBox[0].id;
  } else { // for dotplots
    var polygonId = polygonBox[polygonBox.length -1].id;
  }

  var idLine = polygonId.substring(0, polygonId.lastIndexOf('.'));

  for (i = 1; i <= polygonBox.length; i ++) {
    if (polygonBox[i-1].id.indexOf(idLine) >= 0){
      polygonBox[i-1].setAttribute('class', 'box');
    }
  }
  //var boxes = document.getElementsByClassName('box');
  //return(box);
}

//functions to create boxLabels:
function boxLabel(textinput) {

var boxLabel = document.createElementNS("http://www.w3.org/2000/svg", "text");
  boxLabel.setAttributeNS(null, 'id', textinput);
  boxLabel.setAttributeNS(null, 'class', 'label boxData hidden');
  boxLabel.setAttributeNS(null, 'transform', 'translate(' + Number(x) + ',' + (Number(y) + 10) + ') scale(1, -1)');

  var textNode = document.createTextNode(textinput);
  boxLabel.appendChild(textNode);
  var panel = document.getElementsByTagName('g')[0];
  panel.appendChild(boxLabel);
};

function boxLabelSet(p, r, q, textinput) {
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


// Boxplot Interactions:
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

//hide box:
function hideBox() {
  for (i =0; i < boxData.length; i++) {
    boxData[i].classList.add('hidden');
  }
}

/* ------------------------------------------
FUNCTIONS FOR INTERACTING WITH PLOT OBJECTS
plotObjects: bar, hexbin, histBar, point.
--------------------------------------------*/
light = function(i, className) {
  var plotObject = document.getElementById(Grob + '.' + i);
  plotObject.classList.add(className);

  var gLabel = document.getElementById('gLabel' + i);
  gLabel.classList.remove('invisible');
};

normal = function(i, className) {
  var plotObject = document.getElementById(Grob + '.' + i);
  plotObject.classList.remove(className);

  var gLabel = document.getElementById('gLabel' + i);
  gLabel.classList.add('invisible');
};

/* ------------------------------------------
FUNCTIONS FOR INTERACTING WITH LEGEND
--------------------------------------------*/
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

/* ------------------------------------------
FUNCTIONS FOR HIGHLIGHTING TABLE ROWS
--------------------------------------------*/
// highlight rows in table corresponding to selection (link to table):
returnRowSelection = function(lp, dataRow) {
  dataRow.style.backgroundColor = "rgba" + lp + ", 0.25)";
  dataRow.classList.remove('hidden');
  dataRow.classList.add('rowSelect');
}

omitRowSelection = function(dataRow) {
  dataRow.classList.remove('rowSelect');
  dataRow.classList.add('hidden');
  dataRow.style.backgroundColor = "white";
}

resetRowSelection = function(dataRow) {
  dataRow.classList.remove('hidden');
  dataRow.classList.remove('rowSelect');
  dataRow.style.backgroundColor = "white";
}

//for certain tabs/cells:
returnTabSelection = function(lp, data) {
  data.classList.add('tabSelect');
  data.style.backgroundColor = "rgba(" + lp + ",0.5)";
}

resetTabSelection = function(data) {
  data.classList.remove('tabSelect');
  data.style.backgroundColor = "white";
}

/* ------------------------------------------
FUNCTIONS FOR CREATING SELECTION DRAG BOX
--------------------------------------------*/

//Creating a  (invisible) selection box for users to drag:
createSelectionBox = function(Grob) {
  //putting selection rectangle in a group element:
  var g = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    g.setAttributeNS(null, 'id', 'selectionBox');
    var panel = document.getElementById(Grob);
    panel.appendChild(g);

    //create selectionBox:
  var selectRect = document.createElementNS('http://www.w3.org/2000/svg', 'polygon');
    selectRect.setAttributeNS(null, 'id', 'selectRect');
    selectRect.setAttributeNS(null, 'class', 'selectRect');
    g.appendChild(selectRect);

}

//Create an additional customised selection labels:

createSelectionLabelGroup = function() {
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
      return(selectionLabelGroup);
}

drawSelectRectLabel = function(selectionLabelGroup) {
  var sRectParam = selectionLabelGroup.getBBox();
  selectionLabelRect.setAttribute('x', sRectParam.x-5);
  selectionLabelRect.setAttribute('y', sRectParam.y-2);
  selectionLabelRect.setAttribute('width', sRectParam.width+10);
  selectionLabelRect.setAttribute('height', sRectParam.height+4);
}

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


// Mouse events:
//MouseDown:
MouseDown = function(evt) {

  var selectedGroup = document.getElementById('selectionLabelGroup');
  if (selectedGroup !== null) {
      selectedGroup.remove();
     }

    zoomBox["startX"] = evt.pageX - 20; // 20 comes from the padding added to the body.
    zoomBox["startY"] = evt.pageY - 20;
    zoomBox["isDrawing"] = true;
   selectRect.setAttribute('points',  zoomBox["startX"] + ',' + zoomBox["startY"]);
};

//MouseUp:
MouseUp = function(evt) {
  svg.style.cursor = "default";
      zoomBox["endX"] = evt.pageX - 20;
      zoomBox["endY"] = evt.pageY - 20;
      zoomBox["isDrawing"] = false;
  };

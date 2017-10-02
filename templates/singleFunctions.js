// COMMON FUNCTIONS FOR SINGLE PANEL PLOTS:

//table functions:
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

//FUNCTIONS FOR CREATING LABELS

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

//FUNCTIONS FOR BOXPLOTS

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

//FUNCTIONS FOR INTERACTING WITH LEGEND
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

//FUNCTIONS FOR HIGHLIGHTING TABLE ROWS
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

// Mouse events:
// co-ordinate conversion for svg:
convertCoord = function(svg, evt) {
  var pt = svg.createSVGPoint();
  pt.x = evt.pageX;
  pt.y = evt.pageY;
  return pt.matrixTransform(svg.getScreenCTM().inverse());
}

//MouseDown:
MouseDown = function(evt) {
  var pt = convertCoord(svg, evt);
    zoomBox["startX"] = pt.x;
    zoomBox["startY"] = pt.y;
    zoomBox["isDrawing"] = true;
   selectRect.setAttribute('points',  zoomBox["startX"] + ',' + zoomBox["startY"]);
};

//MouseUp:
MouseUp = function(evt) {
  var pt = convertCoord(svg, evt);
  svg.style.cursor = "default";
      zoomBox["endX"] = pt.x;
      zoomBox["endY"] = pt.y;
      zoomBox["isDrawing"] = false;
  };

  var main   = $("#main"),
      scrw   = main.width (),
      svg    = $("#svg"),    // assuming you give the SVG an ID of 'svg'
      tbl    = $("table"),   // if you have >1 table, select via ID
      svgw   = svg.width (),
      tblw   = tbl.width (),
      gutter = 50;         // leave space between SVG and table

  if (svgw + tblw + gutter > scrw) {
    // add the wide class if the screen is big enough
    main.addClass ('wide');
  }

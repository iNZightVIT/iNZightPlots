// COMMON FUNCTIONS FOR SINGLE PANEL PLOTS:
/* set svg with no width, height */
var svg = document.getElementsByTagName('svg')[0];
svg.removeAttribute('height');
svg.removeAttribute('width');

var data = chart.data;

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
  button.setAttribute("class","btn btn-primary hidden " + name);
  button.innerHTML = "Show " + name;
  button.setAttribute("onclick", "change" + name + "()");
  button.setAttribute('id', 'Button' + name);
  document.getElementById('control').appendChild(button);
};

// TODO: revise further
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

//FUNCTIONS FOR BOXPLOTS
var boxMe = function(lastLine, boxElements) {
  d3.select(boxElements).selectAll('polygon')
    .attr('class', 'box');

    boxLabelSet(0, 1, 0,'LQ');
    boxLabelSet(1, 2, 2, 'UQ');
    boxLabelSet(1, 0, 1, 'Median');
    boxLabelSet(1, 0, 3, 'Min');
    boxLabelSet(2, 1, 4, 'Max');

    //group boxplot together as a single object
    var box = d3.selectAll('.box');
    var boxData = d3.selectAll('.boxData');
    var totalRow = document.getElementById('totalRow');

    //set event listeners:
    box.on("mouseover", function() { box.classed("fillBox", true);})
       .on("mouseout", function() { box.classed("fillBox", false);})
       .on("click", function() { boxData.classed("hidden", false)});
}

//functions to create boxLabels:
function boxLabel(textinput, x ,y) {

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
  var x = boxPoints[0];
  var y = boxPoints[1];

  if (textinput == "Median") { // move median label below the box plot
  var y = boxPoints[1] - 25;
  }

  text = textinput + ": " + chart.boxData[q].quantiles;
  boxLabel(text, x, y);
};

//FUNCTIONS FOR INTERACTING WITH LEGEND
// hover on a legend group:
show = function(i) {
  var keyText = document.getElementById(text[i+3].id);
  if (Grob == "inz-DOTPOINTS.1.1.1.1") {
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
  if (Grob == "inz-DOTPOINTS.1.1.1.1") {
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

// Brush events:
function brushend() {
  if (!d3.event.selection) {
    d3.select(".selection")
      .style("display", "none");
  }
}

function brushstart() {
  d3.select(".selection")
    .style("display", null);
}

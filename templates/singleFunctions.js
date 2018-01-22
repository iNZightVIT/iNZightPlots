// COMMON FUNCTIONS FOR SINGLE PANEL PLOTS:
/* set svg with no width, height */
var svg = document.getElementsByTagName('svg')[0];
svg.removeAttribute('height');
svg.removeAttribute('width');

var data = chart.data;

// make tables not go wide when there's only 1 column
if (chart.type !== "bar") {
  var ncol = $("#table thead tr th").length;
  if (ncol < 3) {
    $('#table').css('width', '50%');
  } else if (ncol <= 4) {
    $('#table').css('width', '75%');
  } else {
    $('#table').css('width', '100%');
  }
} else {
  $('#table').css('width', '100%');
}

//table functions:
// insert an x-header:
insertXHeader = function() {
  var xrow = table.insertRow(0),
      xhead = xrow.insertCell(0);
  xhead.setAttribute('class', 'headings');
  xhead.innerHTML = document.getElementsByTagName('tspan')[2].innerHTML;
  xhead.colSpan = ncol;
};

// creating a y-header:
insertYHeader = function() {
  var yHeading = document.getElementsByTagName('th')[0];
  yHeading.innerHTML = document.getElementsByTagName('tspan')[3].innerHTML;
  yHeading.setAttribute('class',' headings');
}

//create buttons:
button = function(name) {
  var button = document.createElement('button');
  button.setAttribute("type", "button");
  button.setAttribute("class","btn btn-primary hidden " + name);
  button.innerHTML = "Show " + name;
  button.setAttribute("onclick", "change" + name + "()");
  button.setAttribute('id', 'Button' + name);
  document.getElementById('control').appendChild(button);
};

// TODO: revise further
getGrob = function(chartType) {
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
boxMe = function(levelNo) {

  var polygonBoxes = document.querySelectorAll('g[id^="inz-box."]')
  var boxLines = document.querySelectorAll('g[id^="inz-box-line."]')

  for (i = 1; i <= levelNo; i++) { //separate boxes for each plot
    var polygonBox = polygonBoxes[i-1].children;
    var boxLine = boxLines[i-1].children;
    for (j=0; j < 2; j++) {
      polygonBox[j].setAttribute('class', 'box-' + i);
      boxLine[j].setAttribute('class', 'box-' + i);
    }
    boxLabelSet(i, 0, 1, 0,'LQ');
    boxLabelSet(i, 1, 2, 2, 'UQ');
    boxLabelSet(i, 1, 0, 1, 'Median');
    boxLabelSet(i, 1, 0, 3, 'Min');
    boxLabelSet(i, 2, 1, 4, 'Max');
  };

  //Box Plot interactions:
  for (j = 1; j <= levelNo; j++) {
  var box = document.getElementsByClassName('box-' + j);
  for (i = 0; i < box.length; i++) {
    box[i].setAttribute('onmouseover', 'fillBox(' + j + ')');
    box[i].setAttribute('onmouseout', 'normalBox(' + j + ')');
    box[i].setAttribute('onclick', 'showBox(' + j + ')');
    }
  }

  fillBox = function(j) {
    d3.selectAll('.box-' + j)
      .classed('fillBox', true);
  };

  normalBox = function(j) {
    d3.selectAll('.box-' + j)
      .classed('fillBox', false);
    };

  showBox = function(j) {
    var boxData = d3.selectAll('.boxData-' + j);
      boxData.classed('hidden', !boxData.classed('hidden'));
  }
}

//functions to create boxLabels:
boxLabel = function(i, textinput, panel, x, y) {
var boxLabel = document.createElementNS("http://www.w3.org/2000/svg", "text");
  boxLabel.setAttribute('class', 'label boxData-' + i + ' hidden'); //hidden
  boxLabel.setAttributeNS(null, 'transform', 'translate(' + Number(x) + ',' + (Number(y) + 2) + ') scale(1, -1)');
  boxLabel.setAttributeNS(null, 'id', textinput);

  var textNode = document.createTextNode(textinput);
  boxLabel.appendChild(textNode);
  //var panel = document.getElementById('inz-' + tag + '.1.1.' + i + ".1");
  panel.appendChild(boxLabel);
};

boxLabelSet = function(i, p, r, q, textinput) {
  if (textinput == "Min" ||  textinput == "Max") {
    var line = document.getElementById('inz-box-line.1.1.' + i  + '.1.' + p); //i is levelNo.
    // p will either be 1 or 2 -> 1 = minLine, 2 = maxLine
    line.setAttribute('class', 'box-' + i);
    var boxPoints = line.getAttribute('points').split(" ")[r].split(",");
    var panel = line.parentNode;
  } else {
    var box = document.getElementsByClassName('box-' + i)[p];
    // boxplot split into two boxes - lowerbox (p = 0) and upperbox (p = 1)
    var boxPoints = box.getAttribute('points').split(" ")[r].split(",");
    var panel = box.parentNode;
  }
  x = boxPoints[0];
  y = boxPoints[1];

  if (textinput == "Median") {
    // move median label below the box plot
   y = boxPoints[1] - 11;
  }
  if (levelNo == 1) {
    text = textinput + ": " + chart.boxData[q].quantiles;
  } else {
    text = textinput + ": " + chart.boxData[i-1][q].quantiles;
  }
  // this is associated with the boxData imported from R.
  //q = 0 (LQ), 1 (UQ), 2 (Median), 3 (Min), 4 (Max)
  boxLabel(i, text, panel, x, y);
};

//FUNCTIONS FOR INTERACTING WITH LEGEND
// hover on a legend group:
show = function(i) {
  var keyText = document.getElementById('inz-leg-txt-' + i + '.1.1.tspan.1');
  var key = document.getElementById('inz-leg-pt-' + i + '.1.1');
  if (key.getAttribute("fill") !== "none") {
        keyText.setAttribute('fill', key.getAttribute('fill'));
    } else {
        keyText.setAttribute('fill', key.getAttribute('stroke'));
    }
  keyText.setAttribute('class', 'show');
  key.setAttribute('class', 'show');
};

//hover out:
out = function(i) {
  var keyText = document.getElementById('inz-leg-txt-' + i + '.1.1.tspan.1');
  var key = document.getElementById('inz-leg-pt-' + i + '.1.1');
  keyText.setAttribute('class', 'out keyText');
  key.setAttribute('class', 'out');
};

//FUNCTIONS FOR HIGHLIGHTING tabs/cells:
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

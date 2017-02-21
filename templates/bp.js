/* JS code for one way (excludes stacked - refer to other file) and
two way bar plots.
 Code is split in 3 sections: table properties,
                              bar and label properties,
                              and interaction code.
Browser compatibility: Chrome 56, FireFox 38.3 and later
                        Edge, IE11, 10, 9 (unavailable for earlier versions) */

 // Parsing data:
 var prop = JSON.parse(prop);
 var counts = JSON.parse(counts);

/*-----------------------------------------------------
                  Table properties:
Code to label table cells (with classes or ids), rows,
columns. Additional headings, conversion to counts and
percentage buttons are all coded and dynamically inserted
in HTML via JS.
Includes buttons relating to table (counts and percentage
conversion, 'View Table' button).
-----------------------------------------------------*/

//defining table element
var table = document.getElementById('table');
table.style.display = "none";

//no. of columns in table
var tr = table.getElementsByTagName('tr');
ncol = tr[0].childElementCount;

//no. of cells in table
var td = table.getElementsByTagName('td');
cellNo = td.length;

//no. of rows in table
nrow = document.getElementById('table').rows.length;
for (i = 1; i < nrow; i ++) {
  tr[i].setAttribute('id', 'tr' + i);
};

//actual data numbers
dataNo = cellNo - 2*(nrow-1)

//finding no. of rows, and labelling
for (i = 1; i <= cellNo; i ++) {
  if (i % ncol == 0) {
    td[i-1].setAttribute('id', 'tc' + i/ncol);
  } else if (i % ncol == 1){
    td[i-1].setAttribute('id', 'yGroup' + ((i-1)/ncol + 1));

  } else if (prop[0].Var1 != undefined) { // for two way bar plots - different table
      td[i-1].setAttribute('id', 'td' + (((i- (i%ncol))/ncol + 1)+((nrow-1)*(i%ncol-2))));
  } else {
    if( i < (ncol-1)) {
  td[i-1].setAttribute('class', 'td' + (i-1));
} else {
  td[i-1].setAttribute('class', 'td' + ((i-2)%ncol+1));
}
}
};

for (i =1; i <= (nrow-1); i ++) {
var tc = document.getElementById('tc' + i);
tc.style.fontWeight = "bold";
}

var xrow = table.insertRow(0);
var xhead = xrow.insertCell(0);
xhead.innerHTML = document.getElementsByTagName('tspan')[2].innerHTML;
xhead.style.fontStyle = "italic";
xhead.style.textDecoration = "underline";
xhead.colSpan = ncol;
xhead.style.textAlign = "center";

// if two way bar plot: additional conversion to counts, percentages, summation and headers
if (prop[0].Var1 != undefined) {

  //Finding the sum of countsTab:
  var countsTab = new Array();
  for (i = 1; i <= cellNo/ncol; i++) {
    var tc = document.getElementById('tc' + i);
    countsTab[i-1] = Number(tc.innerHTML);
  };

  var sum = countsTab.reduce(function(a, b) { return a + b; }, 0);

  //yHeader:
  for (i = 1; i <=ncol; i++) {
    tableHeader = document.getElementsByTagName('th');
    tableHeader[i-1].setAttribute('id', 'th' + (i-1));
  };

  var yHeading = document.getElementById('th' + 0);
  yHeading.innerHTML = document.getElementsByTagName('tspan')[3].innerHTML;
  yHeading.setAttribute('style', 'font-style: italic');
  yHeading.style.fontWeight = "normal";
  yHeading.style.textDecoration = "underline";

  //Summation row:
  lastRow = table.insertRow(nrow+1);
  for (i = 1; i <= ncol; i ++) {
    var totalCell = lastRow.insertCell(i-1);
    totalCell.id = "totalCell" + i;
    totalCell.align = "center";
  };

  sumCell = document.getElementById('totalCell' + (ncol));
  sumCell.innerHTML = "n = " + sum;
  sumCell.style.fontWeight = "bold";

  totalCell = document.getElementById('totalCell' + (ncol-1));

//Creating buttons:
button = function(name, color) {
  var button = document.createElement('button');
  button.setAttribute("type", "button");
  button.setAttribute("class","btn btn-info");
  button.innerHTML = "Change to " + name;
  button.setAttribute("onclick", "change" + name + "()");
  button.style.backgroundColor = color;
  button.style.marginLeft = "10px";
  button.style.display = "none";
  button.setAttribute("id", "Button" + name)
  document.getElementById('control').appendChild(button);
};

  button("Percentage", "blue");
  button("Count", "purple");

    //Conversion to percentages:
  changePercentage = function() {
    for (j = 1; j <= cellNo/ncol; j++) {
      var tr = document.getElementById('tr' + j);
    for (i = 1; i <= cellNo - 2*(nrow-1); i ++) {
        var td = document.getElementById('td' + i);
        if ((td.innerHTML.indexOf(".") >= 0) && (td.innerHTML.indexOf('%') == -1)) {
        td.innerHTML = (td.innerHTML*100).toFixed(2) + "%";
      } else if (td.innerHTML.indexOf('%') >= 0) {
        td.innerHTML = td.innerHTML;
      } else {
          if(tr.contains(document.getElementById('tc' + j)) && tr.contains(document.getElementById('td' + i))) {
          td.innerHTML = (Number(td.innerHTML)/countsTab[j-1] * 100).toFixed(2) + '%';
          var tc = document.getElementById('tc' + j);
          tc.innerHTML = countsTab[j-1];
          }
        }
      }
    }
    document.getElementsByTagName('th')[(ncol-1)].innerHTML = "Row N";
    totalCell.innerHTML = "";
    sumCell.innerHTML = "n = " + sum;

  };

  //Conversion to counts:
  changeCount = function() {
  for (j = 1; j <= cellNo/ncol; j ++) {
    var tr = document.getElementById('tr' + j);
    for(i = 1; i <= cellNo - 2*(nrow-1); i++) {
  if(tr.contains(document.getElementById('tc' + j)) && tr.contains(document.getElementById('td' + i))) {
        var td = document.getElementById('td' + i);
        var tc = document.getElementById('tc' + j);
        if (td.innerHTML.indexOf('%') >= 0){
        td.innerHTML = Math.round(Number(td.innerHTML.substring(0,td.innerHTML.lastIndexOf('%')))/100 * countsTab[j-1]);
        tc.innerHTML = (countsTab[j-1]/sum*100).toFixed(2) + "%";
      } else if ((td.innerHTML.indexOf(".") >= 0) && (td.innerHTML.indexOf('%') == -1)){
        td.innerHTML = Math.round(Number(td.innerHTML) * countsTab[j-1]);
        tc.innerHTML = (countsTab[j-1]/sum*100).toFixed(2) + "%";
      } else {
        td.innerHTML = td.innerHTML;
        tc.innerHTML = tc.innerHTML;
      }
    }
  }
  }
    document.getElementsByTagName('th')[(ncol-1)].innerHTML = "Row N %";
    totalCell.innerHTML = "n = " + sum;
    sumCell.innerHTML = "100.00%";
  };

};

//drive the viewTable button:
  viewTable = document.getElementById('viewTable');
  t = true;

showTable = function() {
  if(t) {
    viewTable.innerHTML = "Hide Table";
    table.style.display =  "table";
    if(document.getElementById('ButtonPercentage') != undefined) {
    ButtonPercentage.style.display = "inline";
    ButtonCount.style.display = "inline";
  }
    t = false;
  } else {
    viewTable.innerHTML = "View Table";
    table.style.display = "none";
    if(document.getElementById('ButtonPercentage') != undefined) {
    ButtonPercentage.style.display = "none";
    ButtonCount.style.display = "none";
      }
    t = true;
  }
};


/* ---------------------------------------------------------------------------
BarPlot - code below here assigns and create labels for each bar, and defines
          some of their properties.
Anything with prop[0].Var1 != undefined -> signifies two way bar plots.
------------------------------------------------------------------------------ */

// Adding some padding:
document.body.style.padding = "20px";
var svg = document.getElementsByTagName('svg')[0];

//Increasing plotRegion out to show labels:
rect = document.getElementsByTagName('rect')[0];
if (rect.x.baseVal.value < 48) { //there's some redundant rectangle that appears on two-way bar plots.
  var rect = document.getElementsByTagName('rect')[1];
};

rect.setAttribute('width', rect.getAttribute('width')*1.5);
rect.setAttribute('height', rect.getAttribute('height')*1.5);
rect.setAttribute('x', rect.getAttribute('x') - 20);


// obtaining values from SVG file:
var  p = document.getElementsByTagName("polygon");
var id = p[p.length-1].getAttribute('id');
var Grob = id.substring(0, id.lastIndexOf('.'));
  number = counts.length;
  var count = number + 1;


//if the bar plot is colored - has additional bars and polylines to hide!
if (colorMatch != undefined) {
for (i = 0; i < p.length; i++) {
  if (colorMatch[i+1] == 1) {
 p[i].style.visibility = "visible";
   p[i].id = Grob + "." + (i/(number-1));
   p[i].setAttribute("class", "bar");
} else {
 p[i].style.visibility = "hidden";
 }
}

//Hiding polylines:
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
   lines[i].style.visibility = "hidden";
 } else {
   lines[i].style.visibility = "visible";
 }
}
};


// Construction of text labels:
var panel = document.getElementsByTagName('g')[0];

// g elements to group labels together:
gLabel = function(Grob, i) {

var gEl = document.createElementNS("http://www.w3.org/2000/svg", "g");
    gEl.setAttributeNS(null, 'id', 'gLabel' + i);
    gEl.setAttributeNS(null, 'visibility', 'hidden');
    panel.appendChild(gEl);
  };

for (i = 1; i <= count; i++) {
  gLabel(Grob, i);
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

for (i = 1; i <= count; i++) {
  gRect(i);
}

//Function to create individual labels
label = function(Grob, id, textinput, i, tf) {

//attributes for the text SVG element
  var label = document.createElementNS("http://www.w3.org/2000/svg", "text");
    label.setAttributeNS(null, 'x', '0');
    label.setAttributeNS(null, 'y', '0');
    label.setAttributeNS(null, 'font-size', "12");
    label.setAttributeNS(null, 'fill', 'black');
    label.setAttributeNS(null, 'stroke', 'none');
    label.setAttributeNS(null, 'fill-opacity', '1');
    label.setAttributeNS(null, 'text-anchor', 'middle');
    label.setAttributeNS(null, 'visibility', 'inherit');
    label.setAttributeNS(null, 'transform', 'translate('+ ((x+sx)/2) + ', ' + (y + tf) +') scale(1, -1)');
    label.setAttributeNS(null, 'id', id + i);

// Creating the text label element:
  var text = textinput;
  var textNode = document.createTextNode(text);

    label.appendChild(textNode);
    var gLabel = document.getElementById('gLabel' + i);
    gLabel.appendChild(label);
};

//labels generated using function label() + additional information for co-ordinates
for (i  = 1; i < count; i++) {
  var bar = document.getElementById(Grob + '.' + i);
  bar.setAttribute('stroke', 'none');
  var coords = bar.getAttribute('points');
  var small = coords.split(" ")[0];
  var sx = Number(small.split(",")[0]);
  var sy = Number(small.split(",")[1]);
  var coordsxy = coords.split(" ")[2];
  var x = Number(coordsxy.split(",")[0]); //co-ordinates based upon SVG elements.
  var y = Number(coordsxy.split(",")[1]);

  if (prop[0].Var1 != undefined) { //for two way bar plots
    var freq = counts[i-1].Freq;
    var pp = prop[i-1].Freq;
    var gOne = counts[i-1].Var1;
    var gTwo = counts[i-1].Var2;
    var p = 30;
  } else { // for one way bar plots
  var freq = counts[i-1].Freq;
  var pp = prop[i-1].V1;
  var gOne = counts[i-1].Var1;
  var gTwo = ' ';
  };

//position of text labels:
    if (y-sy < 60) {
    var p = 30;
    var q = 45;
    var r = 60;
  } else {
    var p = -60;
    var q = -45;
    var r = -30;
  };

  label(Grob, 'label', (Number(pp)*100).toFixed(2) + "%", i, p);

  //making count labels:
    label(Grob, 'countLabel' + gOne + gTwo, "N = " + freq, i, q);

  //making group labels:
  label(Grob, 'groupLabel' + gOne + gTwo, gOne + ' ' +  gTwo, i, r);
      var groupLabel = document.getElementById('groupLabel' + gOne + gTwo + i);
      groupLabel.style.fontWeight = "bold";

      // Attach and draw rectangles to labels according to the size of the gLabel (with all labels attached)
        var gLabel = document.getElementById('gLabel' + i);
        rectParam = gLabel.getBBox();
        var gRect = document.getElementById('gRect' + i);
        gRect.setAttribute('x', rectParam.x-10);
        gRect.setAttribute('y', rectParam.y-10);
        gRect.setAttribute('width', rectParam.width+20);
        gRect.setAttribute('height', rectParam.height + 20);



};


/// INTERACTION CODE: Hovers, Clicks, Legends
//Hovers on bars and labels:
for (i = 1; i < count; i++) {
 var bar = document.getElementById(Grob + '.' + i);
 bar.setAttribute('onmouseover', 'light('+ i + ')');
 bar.setAttribute('onmouseout', 'normal(' + i +')');
 bar.setAttribute('onclick', 'fade(' + i +')');
 }

 light = function(i) {
   var bar = document.getElementById(Grob + '.' + i);
   bar.setAttribute('style', 'fill-opacity: 0.75');
 };

 normal = function(i) {
   var bar = document.getElementById(Grob + '.' + i);
   bar.setAttribute('style', 'fill-opacity: 1');

 };

//table interaction:

fade = function(i) {
  for (j = 1; j < count; j ++) { //rows differ for twowayBP.

    var bar = document.getElementById(Grob + '.' + j);
    var gLabel = document.getElementById('gLabel' + j);

  if(prop[0].Var1 != undefined) {
    var row = document.getElementById('tr' + ((j-1)%(nrow-1)+1));
    row.style.backgroundColor = "white";
    row.style.fontWeight = "normal";

    data = document.getElementById('td' + j); //it differs due to different formation of tables.

    if (i == j) {
      bar.setAttribute('opacity', '1');
      gLabel.setAttribute('visibility', 'visible');
      data.setAttribute('style', 'font-weight:bold');
      var l = bar.getAttribute('fill');
      lp = l.substring(4, l.length-1);
      data.style.backgroundColor = "rgba(" + lp + ",0.5)";

    } else {
      bar.setAttribute('opacity', '0.3');
      gLabel.setAttribute('visibility', 'hidden');
      data.setAttribute('style', 'font-weight: normal');
      data.style.backgroundColor = "white";

    }

  }  else {

    data = document.getElementsByClassName('td' + j);

    if (i == j) {
      bar.setAttribute('opacity', '1');
      gLabel.setAttribute('visibility', 'visible');

      for (k = 0; k <= 1; k++) { //highlights both counts and percentage at the same time
        data[k].style.fontWeight = "bold";
        var l = bar.getAttribute('fill');
        lp = l.substring(4, l.length-1);
        data[k].style.backgroundColor = "rgba(" + lp + ",0.5)";
      }

    } else {

      gLabel.setAttribute('visibility', 'hidden');

      for (k = 0; k <= 1; k++) {
        data[k].style.fontWeight = "normal";
        data[k].style.backgroundColor = "white";
      }
    }
  }
}
};


// For one way colored plots and two-way bar plots (where a legend is made on the right)
if (prop[0].Var1 != undefined) {
  var percent = JSON.parse(percent); // this is to find the no. of groups for the second variable.
  var group = percent.length +1;
} else {
  var group = count;
};

if (colorMatch != undefined || prop[0].Var1 != undefined) {
//LEGEND INTERACTION:
//grabbing keys and text from the legend:
var keys = document.getElementsByTagName('use');
var text = document.getElementsByTagName('text');

//Legend interaction:
for (i = 1; i < group; i ++) {
  key = document.getElementById(keys[i-1].id);
  keyText = document.getElementById(text[i+3].id);
  keyText.setAttribute('onmouseover', 'show(' + i +')');
  key.setAttribute('onmouseover', 'show(' + i + ')');
  keyText.setAttribute('onmouseout', 'out(' + i +')');
  key.setAttribute('onmouseout', 'out('+ i +')');
  keyText.setAttribute('onclick', 'info(' + i + ')');
  key.setAttribute('onclick', 'info(' + i + ')');
};

show = function(i) {
  var keyText = document.getElementById(text[i+3].id);
  var key = document.getElementById(keys[i-1].id);
  keyText.setAttribute('style', 'fill:' + key.getAttribute('fill'));
  keyText.setAttribute('font-size', '115%');
  key.setAttribute('fill-opacity', '0.5');
};

out = function(i) {
  var keyText = document.getElementById(text[i+3].id);
  var key = document.getElementById(keys[i-1].id);
  keyText.setAttribute('style', 'fill: black');
  keyText.setAttribute('font-size', '100%');
  key.setAttribute('fill-opacity', '1');
};


info = function(i) {
  for (j = 1; j < count; j++) {
      var bar = document.getElementById(Grob + '.' + j);
      var l = bar.getAttribute('fill');
      lp = l.substring(4, l.length-1);
      var gLabel = document.getElementById('gLabel' + j);

if (prop[0].Var1 != undefined) {
  var row = document.getElementById('tr' + ((j-1)%(nrow-1)+1));
  var td = document.getElementById('td' + j);
    td.setAttribute('style', 'inherit');

  if ((j == i || (j%(group-1)) == i || (j%(group-1)) == 0 && i == (group-1))) { //for two-way bar plots

    bar.setAttribute('visibility', 'visible');
    bar.setAttribute('opacity', '1');

    gLabel.setAttribute('visibility', 'visible');

    row.style.fontWeight = "bold";
    row.style.backgroundColor = "rgba(" + lp + ",0.5)";

      } else {

      bar.setAttribute('opacity', '0.3');

      gLabel.setAttribute('visibility', 'hidden');

      row.style.fontWeight = "normal";
      row.style.backgroundColor = "white";

     }
  } else { //for one way colored bar plots:

      var data = document.getElementsByClassName('td' + j);

if (i == j) {
  bar.setAttribute('visibility', 'visible');
  bar.setAttribute('opacity', '1');

  gLabel.setAttribute('visibility', 'visible');

  for (k = 0; k <= 1; k++) {
    data[k].style.fontWeight = "bold";
    data[k].style.backgroundColor = "rgba(" + lp + ",0.5)";
  }
    } else {
    bar.setAttribute('opacity', '0.3');
    gLabel.setAttribute('visibility', 'hidden');

    for (k = 0; k <= 1; k++) {
      data[k].style.fontWeight = "normal";
      data[k].style.backgroundColor = "white";
      }
    }
  }
}
}
};

 //Reset Button: attempts to bring plot back to original state
reset = function() {
   for (i = 1; i < count; i++) {
   var bar = document.getElementById(Grob + '.' + i);
     bar.setAttribute('visibility', 'visible');
     bar.setAttribute('opacity', '1');

     var gLabel = document.getElementById('gLabel' + i);
     gLabel.setAttribute('visibility', 'hidden');

     if(prop[0].Var1 != undefined) { // for two way bar plots
       var row = document.getElementById('tr' + ((i-1)%(nrow-1)+1));
        var td = document.getElementById('td' + i);
        td.setAttribute('style', 'inherit');

       row.style.backgroundColor = "white";
       row.style.fontWeight = "normal";


     } else { // for one way bar plots
     var data = document.getElementsByClassName('td' + i);
     for (k = 0; k <= 1; k++) {
       data[k].style.fontWeight = "normal";
       data[k].style.backgroundColor = "white";
     }
 }
 }
 table.style.display = "none";
 viewTable.innerHTML = "View Table";
 t = true;
 };

/*-------------------------------------------------------------------
JS code for one way stacked bar plots:
 Code is split in 3 sections: table properties (from 27),
                              bar and label properties (from 188),
                              and interaction code (from 330).
 -------------------------------------------------------------------*/

//Parsing JSON data:
var prop = JSON.parse(prop);
var counts = JSON.parse(counts);
var colorCounts = JSON.parse(colorMatch);
var order = JSON.parse(order);

/*------------------------------------------------------------------
                  Table properties:
Code to label table cells (with classes or ids), rows,
columns. Additional headings, conversion to counts and
percentage buttons are all coded and dynamically inserted
in HTML via JS.

Note: this is slightly different to the 2 way table of count due
to the stacking nature and different information being displayed.
In 2 way table of count: rows add to 100%, while here columns add
to 100%.
-------------------------------------------------------------------*/

var table = document.getElementById('table');

//no. of columns in table
var tr = document.getElementsByTagName('tr');
ncol = tr[0].childElementCount;

//no. of cells in table
var td = document.getElementsByTagName('td');
cellNo = td.length;

//no. of rows in table
nrow = document.getElementById('table').rows.length;
for (i = 1; i < nrow; i ++) {
  if(i == nrow-2) { // last two rows are the rows containing totals and counts
    tr[i].setAttribute('class', 'tc');
  } else if (i == nrow-1) {
    tr[i].setAttribute('class', 'countRow');
} else {
    tr[i].setAttribute('class', 'row' + i);
  }
};

//finding no. of rows, and labelling
for (j = 1; j <ncol; j++) {
for (i = 1; i <= cellNo; i ++) {

  if (i % ncol == 1){
    td[i-1].setAttribute('id', 'yGroup' + ((i-1)/ncol + 1));
    td[i-1].setAttribute('class', 'yGroup');
  } else if (document.getElementsByClassName('countRow')[0].contains(td[i-1])){
    td[i-1].setAttribute('id', 'counts' + ((i-1)%ncol));
    td[i-1].innerHTML = Number(td[i-1].innerHTML);

  } else if (document.getElementsByClassName('tc')[0].contains(td[i-1])) {
    td[i-1].setAttribute('id', 'tc' +((i-1)%ncol));
    td[i-1].setAttribute('class', 'tc');
  }
  else {
  td[i-1].setAttribute('id',  'td' + i);
  td[i-1].setAttribute('class', 'td' + order[i-Math.ceil(i/ncol)-1]);
}
}
};

//Finding the sum of countsTab:
var countsTab = [];
for (j = 1; j < ncol; j++) {
    var totalCounts = document.getElementById('counts' + j); //counts for each group
    countsTab[j-1] = Number(totalCounts.innerHTML);
};

var sum = countsTab.reduce(function(a, b) { return a + b; }, 0);


//Inserting table headers:
var xrow = table.insertRow(0);
var xhead = xrow.insertCell(0);
xhead.setAttribute('class', 'headings');
xhead.innerHTML = document.getElementsByTagName('tspan')[2].innerHTML;
xhead.colSpan = ncol;

//yHeader:
var yHeading = document.getElementsByTagName('th')[0];
yHeading.innerHTML = document.getElementsByTagName('tspan')[3].innerHTML;
yHeading.setAttribute('class',' headings');


//Creating buttons and setting attributes:
button = function(name) {
  var button = document.createElement('button');
  button.setAttribute("type", "button");
  button.setAttribute("class","btn btn-primary hidden Button" + name);
  button.innerHTML = "Show " + name;
  button.setAttribute("onclick", "change" + name + "()");
  button.setAttribute('id', 'Button' + name);
  document.getElementById('control').appendChild(button);
};

button("Percentage");
button("Count");

  //Conversion to percentages:
changePercentage = function() {
  var buttonPercentage = document.getElementById('ButtonPercentage');
  var buttonCount = document.getElementById('ButtonCount');
  buttonPercentage.classList.add('dark');
  buttonCount.classList.remove('dark');

  for (i = 1; i <= cellNo; i++) {
      var td = document.getElementById('td' + i);
      var total = document.getElementById('tc' + ((i-1)%ncol)); //will need to be modified once the rev. issue is solved.
      var countsCol = document.getElementById('counts' + ((i-1)%ncol));
      if (td != undefined) {
      if ((td.innerHTML.indexOf(".") >= 0) && (td.innerHTML.indexOf('%') == -1)) { // change proportion to percentages
      td.innerHTML = (td.innerHTML*100).toFixed(2) + "%";
      total.innerHTML = "100.00%"; // this is a bit iffy. Fixed at 100.
      countsCol.innerHTML = countsCol.innerHTML;
    } else if (td.innerHTML.indexOf('%') >= 0) { // remain as percentages
      td.innerHTML = td.innerHTML;
      total.innerHTML = total.innerHTML;
      countsCol.innerHTML = countsCol.innerHTML;
    } else { // change counts to percentages
      td.innerHTML = (Number(td.innerHTML)/countsTab[((i-1)%ncol)-1]*100).toFixed(2) + "%";
      total.innerHTML = "100.00%";
      countsCol.innerHTML = countsTab[((i-1)%ncol)-1];
    }
  document.getElementById('yGroup' + (nrow - 1)).innerHTML = "Col N"; // change from 'Total %' to 'Col N'
}
}
};

//Conversion to counts:
changeCount = function() {
  var buttonPercentage = document.getElementById('ButtonPercentage');
  var buttonCount = document.getElementById('ButtonCount');
  buttonPercentage.classList.remove('dark');
  buttonCount.classList.add('dark');

  for(i = 1; i <= cellNo; i++) {
      var td = document.getElementById('td' + i);
      var total = document.getElementById('tc' + ((i-1)%ncol)); // by columns
      var countsCol = document.getElementById('counts' + ((i-1)%ncol));
      if (td != undefined && total != undefined) {
      if (td.innerHTML.indexOf('%') >= 0){ // convert percentage to counts
      td.innerHTML = Math.round(Number(td.innerHTML.substring(0,td.innerHTML.lastIndexOf('%')))/100 * countsTab[((i-1)%ncol)-1]);
      countsCol.innerHTML = (countsTab[((i-1)%ncol)-1]/sum*100).toFixed(2) + "%";
      total.innerHTML = Math.round(countsTab[((i-1)%ncol)-1]);
    } else if ((td.innerHTML.indexOf(".") >= 0) && (td.innerHTML.indexOf('%') >= -1)){ //converts proportions to counts
      td.innerHTML = Math.round(Number(td.innerHTML) * countsTab[((i-1)%ncol)-1]);
      countsCol.innerHTML = (countsTab[((i-1)%ncol)-1]/sum*100).toFixed(2) + "%";
      total.innerHTML = Math.round(countsTab[((i-1)%ncol)-1]);
    } else { // remain as counts (if already converted to counts)
      td.innerHTML = td.innerHTML;
      total.innerHTML = total.innerHTML;
    }
  }
}
  document.getElementById('yGroup' + (nrow - 1)).innerHTML = "Total %"; // change from 'Col N' to 'total %'
};


//drive the viewTable button:
var viewTable = document.getElementById('viewTable');
  t = true;
showTable = function() {
  if(t) {
    viewTable.innerHTML = "Hide Table";
    table.classList.remove('hidden');
    if (document.getElementById('ButtonPercentage') != (undefined || null)) {
      ButtonPercentage.classList.remove('hidden');
      ButtonCount.classList.remove('hidden');
    }
    t = false;
  } else {
    viewTable.innerHTML = "View Table";
    table.classList.add('hidden');
    if (document.getElementById('ButtonPercentage') != (undefined || null)) {
      ButtonPercentage.classList.add('hidden');
      ButtonCount.classList.add('hidden');
    }
    t = true
  }
};

/*------------------------------------------------------------------
                  Bar plot properties:
Code to identify bars, show and hide lines and appropriate bars.
Addition of appropriate labels to each bar.
-------------------------------------------------------------------*/

//document.body.style.padding = "20px";
var svg = document.getElementsByTagName('svg')[0];

//test rectangle out to show labels:
var rect = document.getElementsByTagName('rect')[1];
rect.setAttribute('class', 'rect');

var count = counts.length*colorCounts.length + 1; //total no. of different combinations + 1
var groups = counts.length; // no. of groups (corresponds to one of the variables)

//Identifying bars
var  p = document.getElementsByTagName('polygon');
var id = p[0].getAttribute('id');
var Grob = id.substring(0, id.lastIndexOf('.'));

for (i = 1; i < p.length; i++) {
  if (p[i].id.indexOf(Grob) >= 0) {
    p[i].classList.add('visible');
  } else {
    p[i].classList.add('hidden');
  }
};


//getting rid of polylines: [ you can delete this if you wish to keep the lines in]
var polyLines = document.getElementsByTagName('polyline');

for (i =1; i < polyLines.length; i ++) {
  if (polyLines[i].id.indexOf("GRID") >= 0) {
    polyLines[i].setAttribute("class", "line");
  }
};

  var lines = document.getElementsByClassName("line");
  var lastId = lines[lines.length-1].id;
  var lastLine = lastId.substring(0, lastId.lastIndexOf('.'));

  for (j = 1; j < lines.length; j ++) {
  if (lines[j].id.indexOf(lastLine) >= 0) {
    lines[j].classList.add('hidden');
  }
};

//creating group labels:
gLabel = function(i) {
var panel = document.getElementsByTagName('g')[0];
var gEl = document.createElementNS("http://www.w3.org/2000/svg", "g");
    gEl.setAttributeNS(null, 'id', 'gLabel' + i);
    gEl.setAttributeNS(null, 'class', 'gLabel invisible');
    panel.appendChild(gEl);
  }

for (i = 1; i < count; i++) {
  gLabel(i);
}

//function to create rectangles for labels:
  gRect = function(i) {
    var gRect = document.createElementNS("http://www.w3.org/2000/svg", "rect");
        gRect.setAttributeNS(null, 'visibility', 'inherit');
        gRect.setAttributeNS(null, 'id', 'gRect' + i);
        gRect.setAttributeNS(null, 'class', 'gRect');
    var gLabel = document.getElementById('gLabel' + i);
    gLabel.appendChild(gRect);
  };

for (i = 1; i < count; i++) {
  gRect(i);
}

//creating labels:
label = function(id, textinput, i, tf) {
//attributes for the text SVG element
  var label = document.createElementNS("http://www.w3.org/2000/svg", "text");
    label.setAttributeNS(null, 'class', 'label' + ' ' + id);
    label.setAttributeNS(null, 'transform', 'translate('+ ((x+sx)/2) + ', ' + (y - tf) +') scale(1, -1)');
    label.setAttributeNS(null, 'id', id + i);

// Creating the text label element:
  var textNode = document.createTextNode(textinput);

    label.appendChild(textNode);
    var gLabel = document.getElementById('gLabel' + i);
    gLabel.appendChild(label);
};

//creating tspan labels - for customizing text in bold:
tLabel = function(id, textinput, i, lab) {
  var tLabel = document.createElementNS("http://www.w3.org/2000/svg", "tspan");
  tLabel.setAttributeNS(null, 'class', 'tLabel');
  tLabel.setAttributeNS(null, 'id', id + i);

  var textNode = document.createTextNode(textinput);
  tLabel.appendChild(textNode);
  lab.appendChild(tLabel);

};

//labels generated using function label() + additional information for co-ordinates
for (j = 1; j <= groups; j++) {
for (i  = 1; i < count; i++) {
  var bar = document.getElementById(Grob + '.' + i);

  var coords = bar.getAttribute('points');
  var small = coords.split(" ")[1];
  var sx = Number(small.split(",")[0]);
  var coordsxy = coords.split(" ")[2];
  var x = Number(coordsxy.split(",")[0]); //co-ordinates based upon SVG elements.
  var y = Number(coordsxy.split(",")[1]);

  if (i%groups == j) {
      var text = counts[j-1].Var1 + "\n" + colorCounts[Math.floor((i+groups-1)/groups-1)]._row;
      label('groupLabel', text, i, 30);

      var text = 'n = ' +  Math.round(eval("colorCounts[Math.floor((i+groups-1)/groups-1)]['" + counts[j-1].Var1 + "']")*counts[j-1].Freq) + ", " ;
      label('countLabel', text, i, 45);

      var text = ((eval("colorCounts[Math.floor((i+groups-1)/groups-1)]['" + counts[j-1].Var1 + "']")*100).toFixed(2)) + "%";
      tLabel('propLabel', text, i , document.getElementById('countLabel' + i));

    }

    if (i%groups == 0 && j == groups) { // for the last bar (closest to the left!)

      var text = counts[groups-1].Var1 + "\n" + colorCounts[Math.floor((i+groups-1)/groups-1)]._row;
      label('groupLabel', text, i, 30);

      var text = 'n = ' +  Math.round(eval("colorCounts[Math.floor((i+groups-1)/groups-1)]['" + counts[groups-1].Var1 + "']")*counts[groups-1].Freq) + ", ";
      label('countLabel',text, i, 45);

      var text = ((eval("colorCounts[Math.floor((i+groups-1)/groups-1)]['"  + counts[counts.length-1].Var1 + "']")*100).toFixed(2)) + "%";
      tLabel('propLabel', text, i, document.getElementById('countLabel' + i));

    }

  }
};

//Attach rectangles to lables + setting styles:
for (i  = 1; i < count; i++) {
// Attach and draw rectangles to labels according to the size of the gLabel (with all labels attached)
  var gLabel = document.getElementById('gLabel' + i);
  rectParam = gLabel.getBBox();
  var gRect = document.getElementById('gRect' + i);
  gRect.setAttribute('x', rectParam.x-10);
  gRect.setAttribute('y', rectParam.y-10);
  gRect.setAttribute('width', rectParam.width+20);
  gRect.setAttribute('height', rectParam.height + 20);
};


/*------------------------------------------------------------------
                  Interaction and event handlers:
Code for mouse events - hovers, clicks on each bar to show labels, and
highlights in table.
Includes function for reset button which attempts to return the plot to
its original state.
-------------------------------------------------------------------*/
// INTERACTION CODE: Hovers, Clicks
//Hovers on bars and labels:
for (i = 1; i < count; i++) {
 bar = document.getElementById(Grob + '.' + i);
 //bar.setAttribute('stroke', 'none');
 bar.setAttribute('onmouseover', 'light('+ i + ')');
 bar.setAttribute('onmouseout', 'normal(' + i +')');
 bar.setAttribute('onclick', 'fade(' + i + ')');
 }

 light = function(i) {
   var bar = document.getElementById(Grob + '.' + i);
   bar.classList.add('light');
   var gLabel = document.getElementById('gLabel' + i);
   gLabel.classList.remove('invisible');
 };

 normal = function(i) {
  var gLabel = document.getElementById('gLabel' + i);
  gLabel.classList.add('invisible');
 };

//table interaction:
fade = function(i) {
  for (j = 1; j < count; j ++) { //individuals
    var bar = document.getElementById(Grob + '.' + j);
    //colors:
    var l = bar.getAttribute('fill');
    var lp = l.substring(4, l.length-1);

    var gLabel = document.getElementById('gLabel' + j);
    var data = table.getElementsByClassName('td' + j)[0];

    if (i == j) {
      gLabel.classList.remove('invisible');

      // Relation to table:
      data.classList.add('tabSelect');
      data.style.backgroundColor = "rgba(" + lp + ",0.5)";

    }  else {
        gLabel.classList.add('invisible');

      //Relation to table
      data.classList.remove('tabSelect');
      data.style.backgroundColor = "white";

    }
  }
};

//Return  - reset button:
 reset = function() {
   for (i = 1; i < count; i++) {

    var gLabel = document.getElementById('gLabel' + i);
    gLabel.classList.add('invisible');

     data = table.getElementsByClassName('td' + i)[0];
     data.classList.remove('tabSelect');
     data.style.backgroundColor = "white";

 }
 table.classList.add('hidden');

var ButtonPercentage = document.getElementById('ButtonPercentage');
var ButtonCount = document.getElementById('ButtonCount');
ButtonPercentage.classList.add('hidden');
ButtonCount.classList.add('hidden'); 

 viewTable.innerHTML = "View Table";
 t = true;
 };

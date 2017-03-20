/* JS code for one way (excludes stacked - refer to other file) and
two way bar plots.
 Code is split in 3 sections: table properties,
                              bar and label properties,
                              and interaction code.     */
 // Parsing data

var prop = JSON.parse(prop),
    counts = JSON.parse(counts);

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
var table = document.getElementById('table'),
    tr = table.getElementsByTagName('tr'),
    ncol = tr[0].childElementCount, //no of columns
    td = table.getElementsByTagName('td'),
    cellNo = td.length, //cell no.
    nrow = table.rows.length, //no. of rows
    dataNo = cellNo - 2 * (nrow - 1), //cells with data
    i,
    svg = document.getElementsByTagName('svg')[0];

for (i = 1; i < nrow; i++) {
    tr[i].setAttribute('id', 'tr' + i);
}

//finding no. of rows, and labelling
for (i = 1; i <= cellNo; i++) {
    if (i % ncol === 0) {
        td[i - 1].setAttribute('id', 'tc' + i / ncol);
        td[i - 1].setAttribute('class', 'tc');
    } else if (i % ncol === 1) {
        td[i - 1].setAttribute('id', 'yGroup' + ((i - 1) / ncol + 1));
        td[i - 1].setAttribute('class', 'yGroup');
    } else if (prop[0].Var1 !== undefined) {
        td[i - 1].setAttribute('id', 'td' + (((i - (i % ncol)) / ncol + 1) + ((nrow - 1) * (i % ncol - 2))));
    } else if (i < ncol) {
        td[i - 1].setAttribute('class', 'td' + (i - 1));
    } else {
        td[i - 1].setAttribute('class', 'td' + ((i - 2) % ncol + 1));
    }
}

//x-header:
var xrow = table.insertRow(0),
    xhead = xrow.insertCell(0);
xhead.setAttribute('class', 'headings');
xhead.innerHTML = document.getElementsByTagName('tspan')[2].innerHTML;
xhead.colSpan = ncol;


// if two way bar plot: additional conversion to counts, percentages, summation and y-headers
if (prop[0].Var1 !== undefined) {

  //Finding the sum of countsTab:
    var countsTab = [];
    for (i = 1; i <= cellNo / ncol; i++) {
        var tc = document.getElementById('tc' + i);
        countsTab[i-1] = Number(tc.innerHTML);
  };

  var sum = countsTab.reduce(function(a, b) { return a + b; }, 0);

  //y heading:
  var yHead = table.getElementsByTagName('th')[0];
  yHead.innerHTML = document.getElementsByTagName('tspan')[3].innerHTML;
  yHead.setAttribute('class', 'headings');


  //Summation row:
  var lastRow = table.insertRow(nrow+1);
  lastRow.setAttribute('class', 'totalRow');
  for (i = 1; i <= ncol; i ++) {
    var cell = lastRow.insertCell(i-1);
    cell.id = "totalCell" + i;
  };

  var sumCell = document.getElementById('totalCell' + (ncol));
  sumCell.innerHTML = "N = " + sum;

  var totalCell = document.getElementById('totalCell' + (ncol-1));

//Creating buttons:
button = function(name) {
  var button = document.createElement('button');
  button.setAttribute("type", "button");
  button.setAttribute("class","btn btn-info convert hidden Button" + name);
  button.innerHTML = "Show " + name;
  button.setAttribute("onclick", "change" + name + "()");
  button.setAttribute("id", "Button" + name)
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
    sumCell.innerHTML = "N = " + sum;

  };

  //Conversion to counts:
  changeCount = function() {

    var buttonPercentage = document.getElementById('ButtonPercentage');
    var buttonCount = document.getElementById('ButtonCount');
    buttonPercentage.classList.remove('dark');
    buttonCount.classList.add('dark');

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
    totalCell.innerHTML = "N = " + sum;
    sumCell.innerHTML = "100.00%";
  };

};


//viewTable button:
  viewTable = document.getElementById('viewTable');
  t = true;

showTable = function() {
  if(t) {
    viewTable.innerHTML = "Hide Table";
    table.classList.remove('hidden');
    if(document.getElementById('ButtonPercentage') != undefined) {
    ButtonPercentage.classList.remove('hidden');
    ButtonCount.classList.remove('hidden');
  }
    t = false;
  } else {
    viewTable.innerHTML = "View Table";
    table.classList.add('hidden');
    if(document.getElementById('ButtonPercentage') != undefined) {
    ButtonPercentage.classList.add('hidden');
    ButtonCount.classList.add('hidden');
      }
    t = true;
  }
};


/* ---------------------------------------------------------------------------
BarPlot - code below here assigns and create labels for each bar, and defines
          some of their properties.
Anything with prop[0].Var1 != undefined -> signifies two way bar plots.
------------------------------------------------------------------------------ */

var svg = document.getElementsByTagName('svg')[0];

//Increasing plotRegion out to show labels:
var rect = document.getElementsByTagName('rect')[0];
if (rect.x.baseVal.value < 48) { //there's some rectangle that appears on two-way bar plots.
  var rect = document.getElementsByTagName('rect')[1];
};
rect.setAttribute('class', 'rect');

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
   p[i].id = Grob + "." + (i/(number-1));
   p[i].classList.add('visible'); // for bars:
} else {
  p[i].classList.add('hidden');

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
   lines[i].classList.add('hidden');
 } else {
   lines[i].classList.add('visible');
 }
}
};

// Construction of text labels:


// g elements to group labels together:
gLabel = function(i) {
var panel = document.getElementsByTagName('g')[0];
var gEl = document.createElementNS("http://www.w3.org/2000/svg", "g");
    gEl.setAttributeNS(null, 'id', 'gLabel' + i);
    gEl.setAttributeNS(null, 'class', 'gLabel invisible');
    panel.appendChild(gEl);
  };

for (i = 1; i <= count; i++) {
  gLabel(i);
};

//function to create rectangles for labels:
  gRect = function(i) {
    var gRect = document.createElementNS("http://www.w3.org/2000/svg", "rect");
      gRect.setAttributeNS(null, 'id', 'gRect' + i);
      gRect.setAttributeNS(null, 'class', 'gRect');

//appending to group element:
    gLabel = document.getElementById('gLabel' + i);
    gLabel.appendChild(gRect);
  };

for (i = 1; i <= count; i++) {
  gRect(i);
}

//Function to create individual labels
label = function(id, textinput, i, tf) {

//attributes for the text SVG element
  var label = document.createElementNS("http://www.w3.org/2000/svg", "text");
    label.setAttributeNS(null, 'class', 'label' + ' ' + id);
    label.setAttributeNS(null, 'transform', 'translate('+ ((x+sx)/2) + ', ' + (y + tf) +') scale(1, -1)');
    label.setAttributeNS(null, 'id', id + i);

// Creating the text label element:
  var text = textinput;
  var textNode = document.createTextNode(text);

//Appending to group element
    label.appendChild(textNode);
    var gLabel = document.getElementById('gLabel' + i);
    gLabel.appendChild(label);
};

//labels generated using function label() + additional information for co-ordinates
for (i  = 1; i < count; i++) {
  var bar = document.getElementById(Grob + '.' + i);
  bar.setAttribute('class', 'bar');
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
    if (y-sy < 100) {
    var p = 30;
    var q = 45;
    var r = 60;
  } else {
    var p = -60;
    var q = -45;
    var r = -30;
  };

  //making group labels:
  label('groupLabel', gOne + ' ' +  gTwo, i, r);

  // proportion labels:
  label('propLabel',(Number(pp)*100).toFixed(2) + "%", i, p);

  // count labels:
    label('countLabel', "n = " + freq, i, q);

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
   var gLabel = document.getElementById('gLabel' + i);
   gLabel.classList.remove('invisible');
 };

 normal = function(i) {
   var gLabel = document.getElementById('gLabel' + i);
   gLabel.classList.add('invisible');
 };

//table interaction:
fade = function(i) {
  for (j = 1; j < count; j ++) { //rows differ for twowayBP.

    var bar = document.getElementById(Grob + '.' + j);
    //colors:
    var l = bar.getAttribute('fill');
    var lp = l.substring(4, l.length-1);

    var gLabel = document.getElementById('gLabel' + j);

  if(prop[0].Var1 != undefined) {
    var row = document.getElementById('tr' + ((j-1)%(nrow-1)+1));
    row.classList.remove('tabSelect');
    row.style.backgroundColor = "white";

    data = document.getElementById('td' + j);
    //it differs due to different formation of tables

    if (i == j) {
      bar.setAttribute('class', 'bar selected');
      gLabel.classList.remove('invisible');

      data.classList.add('tabSelect');
      data.style.backgroundColor = "rgba(" + lp + ",0.5)";


    } else {
      bar.setAttribute('class', 'bar none');
      gLabel.classList.add('invisible');

      data.classList.remove('tabSelect');
      data.style.backgroundColor = "white";

    }

  }  else { // for one way tables.

  data = table.getElementsByClassName('td' + j);

    if (i == j) {
      bar.setAttribute('class', 'bar selected');
      gLabel.classList.remove('invisible');

       for (k = 0; k <= 1; k++) {
         data[k].style.backgroundColor = "rgba(" + lp + ",0.5)";
         if (k == 1) {
           data[k].classList.add('tabSelect');
         }
     }

    } else {
      bar.setAttribute('class', 'bar none');
      gLabel.classList.add('invisible');

      for (k = 0; k <= 1; k++) {
        data[k].classList.remove('tabSelect');
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
  keyText.setAttribute('fill', key.getAttribute('fill'));
  keyText.setAttribute('class', 'show');
  key.setAttribute('class', 'show');

};

out = function(i) {
  var keyText = document.getElementById(text[i+3].id);
  var key = document.getElementById(keys[i-1].id);
  keyText.setAttribute('class', 'out keyText');
  key.setAttribute('class', 'out');

};


info = function(i) {
  for (j = 1; j < count; j++) {
      var bar = document.getElementById(Grob + '.' + j);
      var l = bar.getAttribute('fill');
      lp = l.substring(4, l.length-1);
      var gLabel = document.getElementById('gLabel' + j);
      var gRect = document.getElementById('gRect' + j);

if (prop[0].Var1 != undefined) {
  var row = document.getElementById('tr' + ((j-1)%(nrow-1)+1));
  var td = document.getElementById('td' + j);
    row.classList.remove('tabSelect');
    td.classList.remove('tabSelect');
     td.setAttribute('style', 'inherit');

  if ((j == i || (j%(group-1)) == i || (j%(group-1)) == 0 && i == (group-1))) {
    //for two-way bar plots
    bar.setAttribute('class', 'bar selected');
    gLabel.classList.remove('invisible');
    gRect.classList.add('hidden');

   row.classList.add('tabSelect');
   row.style.backgroundColor = "rgba(" + lp + ",0.5)";

      } else {
      bar.setAttribute('class', 'bar none');
      gLabel.classList.add('invisible');
      gRect.classList.remove('hidden');

      row.classList.remove('tabSelect');
      row.style.backgroundColor = "white";

     }
  } else { //for one way colored bar plots:

      var data = document.getElementsByClassName('td' + j);

if (i == j) {
  bar.setAttribute('class', 'bar selected');
  gLabel.classList.remove('invisible');

  for (k = 0; k <= 1; k++) {
    data[k].classList.add('tabSelect');
    data[k].style.backgroundColor = "rgba(" + lp + ",0.5)";
}
    } else {
    bar.setAttribute('class', 'bar none');
    gLabel.classList.add('invisible');

    for (k = 0; k <= 1; k++) {
      data[k].classList.remove('tabSelect');
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
    bar.setAttribute('class', 'bar');

     var gLabel = document.getElementById('gLabel' + i);
     gLabel.classList.add('invisible');

     var gRect = document.getElementById('gRect' + i);
     gRect.classList.remove('hidden');

     if(prop[0].Var1 != undefined) { // for two way bar plots
       var row = document.getElementById('tr' + ((i-1)%(nrow-1)+1));
        var td = document.getElementById('td' + i);

        row.classList.remove('tabSelect');
        row.style.backgroundColor = "white";

        td.classList.remove('tabSelect');
        td.setAttribute('style', 'inherit');


     } else { // for one way bar plots
     var data = document.getElementsByClassName('td' + i);
     for (k = 0; k <= 1; k++) {
       data[k].classList.remove('tabSelect');
       data[k].style.backgroundColor = "white";
     }
 }
 }
 table.classList.add('hidden');
 var ButtonPercentage = document.getElementById('ButtonPercentage');
 var ButtonCount = document.getElementById('ButtonCount');
 if (ButtonCount !== null) {
 ButtonPercentage.classList.add('hidden');
 ButtonCount.classList.add('hidden');
 }
 viewTable.innerHTML = "View Table";
 t = true;
 };

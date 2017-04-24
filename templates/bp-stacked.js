/*-------------------------------------------------------------------
JS code for one way stacked bar plots:

Note: this is slightly different to the 2 way table of count due
to the stacking nature and different information being displayed.
In 2 way table of count: rows add to 100%, while here columns add
to 100%.
-------------------------------------------------------------------*/

var table = document.getElementById('table');
    tr = document.getElementsByTagName('tr'), //no. of columns in table
    ncol = tr[0].childElementCount,
    td = document.getElementsByTagName('td'),
    nrow = document.getElementById('table').rows.length, //no. of rows in table
    cellNo = td.length; // no. of cells in table

  // labelling rows:
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
insertXHeader();
insertYHeader();


//Creating buttons and conversion functions:
button("Percentage");
button("Count");

  //Conversion to percentages:
changePercentage = function() {

  addClass('ButtonPercentage', 'dark');
  removeClass('ButtonCount', 'dark');

  for (i = 1; i <= cellNo; i++) {
      var td = document.getElementById('td' + i);
      var total = document.getElementById('tc' + ((i-1)%ncol));
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

  addClass('ButtonCount', 'dark');
  removeClass('ButtonPercentage', 'dark');

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
  var t = true;
showTable = function() {
  var viewTable = document.getElementById('viewTable');
  if(t) {
    viewTable.innerHTML = "Hide Table";
    removeClass('table', 'hidden');
    removeClass('ButtonPercentage', 'hidden');
    removeClass('ButtonCount', 'hidden');

    t = false;
  } else {
    viewTable.innerHTML = "View Table";
    addClass('table', 'hidden');
    addClass('ButtonPercentage', 'hidden');
    addClass('ButtonCount', 'hidden');
    t = true
  }
};

/*------------------------------------------------------------------
                  Bar plot properties:
Code to identify bars, show and hide lines and appropriate bars.
Addition of appropriate labels to each bar.
-------------------------------------------------------------------*/

var svg = document.getElementsByTagName('svg')[0],
    rect = document.getElementsByTagName('rect')[1];
rect.setAttribute('class', 'rect');

var count = counts.length*colorMatch.length + 1, //total no. of different combinations
    groups = counts.length; // no. of groups (corresponds to one of the variables)

//Identifying bars

Grob = getGrob('bp-stacked');

//hide underlying bars:
var  p = document.getElementsByTagName('polygon');
for (i = 1; i < p.length; i++) {
  if (p[i].id.indexOf(Grob) >= 0) {
    p[i].classList.add('visible');
  } else {
    p[i].classList.add('hidden');
  }
};


//getting rid of polylines:
hideBarLines();


//creating labels:
for (i = 1; i < count; i++) {
  gLabel(i);
}

for (i = 1; i < count; i++) {
  gRect(i);
}

//labels generated using function label() + additional information for co-ordinates
for (j = 1; j <= groups; j++) {
for (i  = 1; i < count; i++) {
  var bar = document.getElementById(Grob + '.' + i);

  var coords = bar.getAttribute('points');
  var small = coords.split(" ")[1];
  var sx = Number(small.split(",")[0]);
  var coordsxy = coords.split(" ")[2];
  var x = (Number(coordsxy.split(",")[0]) + sx)/2; //co-ordinates of where we want to place label.
  var y = Number(coordsxy.split(",")[1]);

  if (i%groups == j) { //TODO: avoid using eval.
      var text = counts[j-1].Var1 + "\n" + colorMatch[Math.floor((i+groups-1)/groups-1)]._row;
      label('groupLabel', text, i, -30);

      var text = 'n = ' +  Math.round(eval("colorMatch[Math.floor((i+groups-1)/groups-1)]['" + counts[j-1].Var1 + "']")*counts[j-1].Freq) + ", " ;
      label('countLabel', text, i, -45);

      var text = ((eval("colorMatch[Math.floor((i+groups-1)/groups-1)]['" + counts[j-1].Var1 + "']")*100).toFixed(2)) + "%";
      tLabel('propLabel', text, i , document.getElementById('countLabel' + i));

    }

    if (i%groups == 0 && j == groups) { // for the last bar (closest to the left!)

      var text = counts[groups-1].Var1 + "\n" + colorMatch[Math.floor((i+groups-1)/groups-1)]._row;
      label('groupLabel', text, i, -30);

      var text = 'n = ' +  Math.round(eval("colorMatch[Math.floor((i+groups-1)/groups-1)]['" + counts[groups-1].Var1 + "']")*counts[groups-1].Freq) + ", ";
      label('countLabel',text, i, -45);

      var text = ((eval("colorMatch[Math.floor((i+groups-1)/groups-1)]['"  + counts[counts.length-1].Var1 + "']")*100).toFixed(2)) + "%";
      tLabel('propLabel', text, i, document.getElementById('countLabel' + i));

    }

  }
};

//Attach rectangles to lables + setting styles:
for (i  = 1; i < count; i++) {
// Attach and draw rectangles to labels according to the size of the gLabel (with all labels attached)
  drawRectLabel(i);
};


// INTERACTION CODE: Event handlers
//Hovers on bars and labels:
for (i = 1; i < count; i++) {
  (function(i){
    var bar = document.getElementById(Grob + '.' + i);
    bar.addEventListener("mouseover",function(){light(i,'light')},false);
    bar.addEventListener("mouseout", function(){normal(i, 'light')}, false);
    bar.addEventListener("click", function(){fade(i)}, false); //defined below
    }) (i)
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
      returnTabSelection(lp, data);

    }  else {
        gLabel.classList.add('invisible');

      //Relation to table
      resetTabSelection(data);

    }
  }
};

//Return  - reset button:
 reset = function() {
   for (i = 1; i < count; i++) {

    var gLabel = document.getElementById('gLabel' + i);
    gLabel.classList.add('invisible');

     data = table.getElementsByClassName('td' + i)[0];
     resetTabSelection(data);
 }

 addClass('table', 'hidden');
 addClass('ButtonPercentage', 'hidden');
 addClass('ButtonCount', 'hidden');

var viewTable = document.getElementById('viewTable');
 viewTable.innerHTML = "View Table";
 t = true;
 };

//another way to reset:
var plotRegion = document.getElementsByTagName('rect')[2];
plotRegion.addEventListener('click', reset, false);

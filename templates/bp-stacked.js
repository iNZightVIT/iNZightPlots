/*-------------------------------------------------------------------
Stacked bar plots:
Different to the 2 way table of count.
In 2 way table of count: rows add to 100%, while here columns add
to 100%.
-------------------------------------------------------------------*/
var table = document.getElementById('table');
    tr = document.getElementsByTagName('tr'), //no. of columns in table
    ncol = tr[0].childElementCount,
    td = document.getElementsByTagName('td'),
    nrow = document.getElementById('table').rows.length, //no. of rows in table
    colorMatch = chart.colorMatch,
    cellNo = td.length; // no. of cells in table

  // labelling rows:
for (var i = 1; i < nrow; i ++) {
  if(i == nrow-2) { // last two rows are the rows containing totals and counts
    tr[i].setAttribute('class', 'tc');
  } else if (i == nrow-1) {
    tr[i].setAttribute('class', 'countRow');
} else {
    tr[i].setAttribute('class', 'row' + i);
  }
};

//finding no. of rows, and labelling
for (var j = 1; j <ncol; j++) {
  for (var i = 1; i <= cellNo; i ++) {
    if (i % ncol == 1){
      td[i-1].setAttribute('id', 'yGroup' + ((i-1)/ncol + 1));
      td[i-1].setAttribute('class', 'yGroup');
    } else if (document.getElementsByClassName('countRow')[0].contains(td[i-1])){
      td[i-1].setAttribute('id', 'counts' + ((i-1)%ncol));
      td[i-1].innerHTML = Number(td[i-1].innerHTML);
    } else if (document.getElementsByClassName('tc')[0].contains(td[i-1])) {
      td[i-1].setAttribute('id', 'tc' +((i-1)%ncol));
      td[i-1].setAttribute('class', 'tc');
    } else {
      td[i-1].setAttribute('id',  'td' + i);
      td[i-1].setAttribute('class', 'td' + chart.order[i-Math.ceil(i/ncol)-1]);
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
  d3.select(".Percentage")
    .classed("dark", true);
  d3.select(".Count")
    .classed("dark", false);

  for (var i = 1; i <= cellNo; i++) {
      var td = document.getElementById('td' + i);
      var total = document.getElementById('tc' + ((i-1)%ncol));
      var countsCol = document.getElementById('counts' + ((i-1)%ncol));
      if (td !== null) {
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
  d3.select(".Percentage")
    .classed("dark", false);
  d3.select(".Count")
    .classed("dark", true);

  for(i = 1; i <= cellNo; i++) {
      var td = document.getElementById('td' + i);
      var total = document.getElementById('tc' + ((i-1)%ncol)); // by columns
      var countsCol = document.getElementById('counts' + ((i-1)%ncol));
      if (td !== null && total !== undefined) {
      if (td.innerHTML.indexOf('%') >= 0) { // convert percentage to counts
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

//viewTable button:
var t = true;
showTable = function() {
  var viewTable = document.getElementById('viewTable');
  var table = d3.select('table');
  var bpct = d3.select(".Percentage");
  var bct = d3.select(".Count");
  if(t) {
    viewTable.innerHTML = "Hide Table";
    table.classed("hidden", false);
    bpct.classed("hidden", false);
    bct.classed("hidden", false);
    t = false;
  } else {
    viewTable.innerHTML = "View Table";
    table.classed("hidden", true);
    bpct.classed("hidden", true);
    bct.classed("hidden", true);
    t = true;
  }
};

//identify bars:
var count = data.length;
var Grob = getGrob('bp-stacked');

//hide underlying bars:
var p = document.getElementsByTagName('polygon');
for (i = 0; i < p.length; i++) {
  if (p[i].id.indexOf(Grob) >= 0) {
    p[i].classList.add('bar');
  } else {
    p[i].classList.add('hidden');
  }
};

//getting rid of polylines:
var barLines = document.getElementById('inz-bar-line.1.1.1');
d3.select(barLines).selectAll('polyline')
  .classed("hidden", true);

//tooltip:
var tooltip = d3.select('body').append('div')
              .attr('class', 'tooltip')
              .attr('id', 'tooltip')
              .style('width', '100px')
              .style('min-height', '50px');

if (colorMatch !== null) {
    var bars = d3.select('svg').selectAll('.bar');
    var plotRegion = document.getElementsByTagName('rect')[2];
} else {
    var panel = document.getElementById(Grob);
    var plotRegion = document.getElementsByTagName('rect')[1];
    var bars = d3.select(panel).selectAll('polygon');
}

// tooltips:
bars.data(data)
.attr('class', 'bar')
.on('mouseover', function (d) {
    var el = d3.select(this);
    var coords = el.attr('points');
    var small = coords.split(" ")[0];
    var sx = Number(small.split(",")[0]);
    var coordsxy = coords.split(" ")[2];
    var lx = Number(coordsxy.split(",")[0]);
    var xx = lx + (sx-lx)/2 - 50;
    var y = Number(coordsxy.split(",")[1]) + 60;
    //translate to html co-ordinates
    var tm = this.getScreenCTM()
                     .translate(+ xx, + y);

    tooltip.style('visibility', 'visible')
                .style("left", (window.pageXOffset + tm.e)  + "px")
                .style("top", (window.pageYOffset +tm.f)  + "px")
                .html("<span>" + d.Var1 + ' ' + d.Var2 + "<br> " + d.pct +
                "%</span>" + "<br>" +
                d.counts);
})
 .on('mouseout', function (d) {
     tooltip.style('visibility', 'hidden');
 })
.on('click', function (d, i) {
    for (var j = 1; j <= count; j++) { //individuals
        var bar = document.getElementById(Grob + '.' + j);
        //colors:
        var l = bar.getAttribute('fill');
        var lp = l.substring(4, l.length - 1);
        var data = table.getElementsByClassName('td' + j)[0];
        //relate to table
        if ((i+1) == j) {
            returnTabSelection(lp, data);
        } else {
            resetTabSelection(data);
        }
    }
})

//Reset:
 reset = function() {
   for (var i = 1; i <= count; i++) {
     data = table.getElementsByClassName('td' + i)[0];
     resetTabSelection(data);
   }

  d3.select('table')
    .classed('hidden', true);
  d3.select('#Percentage')
    .classed('hidden', true);
  d3.select('#Count')
    .classed('hidden', true);

var viewTable = document.getElementById('viewTable');
 viewTable.innerHTML = "View Table";
 t = true;
 };

//another way to reset:
plotRegion.addEventListener('click', reset, false);

// for bar plots: stacked, two-way and single
// set up table:
var table = document.getElementById('table'),
    tr = table.getElementsByTagName('tr'),
    ncol = tr[0].childElementCount, //no of columns
    td = table.getElementsByTagName('td'),
    cellNo = td.length, //cell no.
    nrow = table.rows.length, //no. of rows
    colorMatch = chart.colorMatch,
    colCounts = chart.colCounts,
    number = data.length,
    count = number + 1;

if (chart.type == "bp-stacked") {
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

} else {
  //defining table elements
  d3.selectAll('tr')
    .attr('id', function(d, i) { return ('tr' + i) });

  //finding no. of rows, and labelling
  for (var i = 1; i <= cellNo; i++) {
      if (i % ncol === 0) {
          td[i - 1].setAttribute('id', 'tc' + i / ncol);
          td[i - 1].setAttribute('class', 'tc');
      } else if (i % ncol === 1) {
          td[i - 1].setAttribute('id', 'yGroup' + ((i - 1) / ncol + 1));
          td[i - 1].setAttribute('class', 'yGroup');
      } else if (data[0].var1 !== undefined) {
          td[i - 1].setAttribute('id', 'td' + (((i - (i % ncol)) / ncol + 1) + ((nrow - 1) * (i % ncol - 2))));
      } else if (i < ncol) {
          td[i - 1].setAttribute('class', 'td' + (i - 1));
      } else {
          td[i - 1].setAttribute('class', 'td' + ((i - 2) % ncol + 1));
      }
    }
}

//x-header:
insertXHeader();

if (data[0].var1 !== undefined || data[0].Var1 !== undefined) {
  //Finding the sum of countsTab:
  countsTab = [];
  if (chart.type == "bar") {
    for (i = 1; i <= cellNo / ncol; i++) {
        var tc = document.getElementById('tc' + i);
        countsTab[i-1] = Number(tc.innerHTML);
    }
  } else {
    for (j = 1; j < ncol; j++) {
        var totalCounts = document.getElementById('counts' + j); //counts for each group
        countsTab[j-1] = Number(totalCounts.innerHTML);
    }
  }

  //global sum variable
  sum = countsTab.reduce(function(a, b) { return a + b; }, 0);

  //y heading:
  insertYHeader();

  if (chart.type == "bar") {
    //Summation row  - should move this to R?
    var lastRow = table.insertRow(nrow+1);
    lastRow.setAttribute('class', 'totalRow');

    for (i = 1; i <= ncol; i ++) {
      var cell = lastRow.insertCell(i-1);
      cell.id = "totalCell" + (i-1);
      cell.setAttribute("class", "totalCell");
      //fill in column totals:
      cell.innerHTML = colCounts[i-1];
    }

    var sumCell = document.getElementById('totalCell' + (ncol-1));
    sumCell.innerHTML = "N = " + sum;
    var totalCell = document.getElementById('totalCell' + (ncol-2));
  }

  //insert buttons and conversion functions:
  button("Percentage");
  button("Count");

}

//Conversion to percentages:
changePercentage = function() {

  d3.select(".Count")
    .classed('dark', false);
  d3.select('.Percentage')
    .classed('dark', true);

  if (chart.type == "bp-stacked") { //stacked
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
      } else { // two way tables
        //for the column sums:
        for (i = 1; i < ncol-2; i++) {
          var tCol = document.getElementById('totalCell' + i);
          if ((tCol.innerHTML.indexOf('%') == -1) && (tCol.innerHTML.indexOf(".") >=0)) {
            tCol.innerHTML = (Number(tCol.innerHTML)*100).toFixed(2) + '%';
          } else if (tCol.innerHTML.indexOf('%') >= 0) {
            tCol.innerHTML = tCol.innerHTML;
          } else {
            tCol.innerHTML =(Number(tCol.innerHTML)/sum * 100).toFixed(2) + '%';
          }
        }

        //for all other data:
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
      totalCell.innerHTML = "100.00%";
      sumCell.innerHTML = "N = " + sum;
    }
};

//Conversion to counts:
changeCount = function() {

    d3.select(".Count")
      .classed('dark', true);
    d3.select('.Percentage')
      .classed('dark', false);

    if (chart.type == "bp-stacked") { // stacked
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
      document.getElementById('yGroup' + (nrow - 1)).innerHTML = "Total %";
    } else { // two way tables
      //for the column sums:
      for (i = 1; i < ncol-2; i++) {
        var tCol = document.getElementById('totalCell' + i);
        if ((tCol.innerHTML.indexOf('%') == -1) && (tCol.innerHTML.indexOf(".") >=0)) {
          tCol.innerHTML = Math.round(Number(tCol.innerHTML)*sum);
        } else if (tCol.innerHTML.indexOf('%') >= 0) {
          tCol.innerHTML = Math.round(Number(tCol.innerHTML.substring(0,tCol.innerHTML.lastIndexOf('%')))/100 * sum);
        } else {
          tCol.innerHTML = tCol.innerHTML;
        }
      }

    //for all other data:
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
  }
};

//initialize DataTable:
var table = $('#table').DataTable({
    "colReorder": true,
    //disable ordering and sorting
    "ordering": false,
    "aaSorting": []
});

//viewTable button:
showTable = function() {
  $('#table_wrapper').toggleClass('hidden');
  $('.Percentage').toggleClass('hidden');
  $('.Count').toggleClass('hidden');
};

// obtaining values from SVG file:

screenBars = function(colorMatch) {

  var Grob = getGrob('bar');
  //if the bar plot is colored - has additional bars and polylines to hide!
  if (colorMatch[0] !== null) {
    var p = document.getElementsByTagName('polygon');
    // if stacked:
    if (chart.type == "bp-stacked") {
      var Grob = getGrob('bp-stacked');
      for (i = 0; i < p.length; i++) {
        if (p[i].id.indexOf(Grob) >= 0) {
          p[i].classList.add('bar');
        } else {
          p[i].classList.add('hidden');
        }
      };
    } else {
      for (i = 0; i < p.length; i++) {
        if (colorMatch[i] == 1) {
          p[i].id = Grob + "." + (i/(number-1));
          p[i].classList.add('bar');
        } else {
          p[i].classList.add('hidden');
      }
    }
  }

  //Hiding polylines:
    var barLines = document.getElementById('inz-bar-line.1.1.1');
    d3.select(barLines).selectAll('polyline')
      .classed("hidden", true);
    var bars = d3.select('svg').selectAll('.bar');

  } else {
    var panel = document.getElementById(Grob),
        bars = d3.select(panel).selectAll('polygon');
  }
  return(bars);
}

//tooltip:
var tooltip = d3.select('body').append('div')
              .attr('class', 'tooltip')
              .attr('id', 'tooltip')
              .style('width', '100px')
              .style('min-height', '40px')
              .style('visibility', 'hidden');

// link bars with tooltips and table:
makeTooltips = function(bars, data) {

  bars.data(data)
      .attr('class', 'bar')
      .on('mouseover', function(d){ var el = d3.select(this),
                                        coords = el.attr('points'),
                                        small = coords.split(" ")[0],
                                        sx = Number(small.split(",")[0]),
                                        coordsxy = coords.split(" ")[2],
                                        x = Number(coordsxy.split(",")[0]),
                                        xx = x + (sx-x)/2 - 50,
                                        y = Number(coordsxy.split(",")[1]) + 60;

                                    //translate to html co-ordinates
                                    var tm = this.getScreenCTM()
                                                     .translate(+ xx, + y);

                                    var tooltip = d3.select("#tooltip");
                                    tooltip.style('visibility', 'visible')
                                                .style("left", (window.pageXOffset + tm.e)  + "px")
                                                .style("top", (window.pageYOffset +tm.f)  + "px"); //position needs fixing
                                    if (data[0].var1 !== undefined) { // for 2-way tables
                                      tooltip.html("<span>" + d.var1 + ' ' + d.var2+ "<br/> " + d.pct +
                                      "%</span>" + "<br/>" + d.counts);
                                    } else if (data[0].Var1 !== undefined) { //for stacked
                                      tooltip.html("<span>" + d.Var1 + ' ' + d.Var2 + "<br> " + d.pct +
                                      "%</span>" + "<br>" +
                                      d.counts);
                                    } else {
                                      tooltip.html("<span>" + d.varx + "<br/> " + d.pct +
                                      "%</span>" + "<br/>" + d.counts);
                                    }
                                  })
      .on('mouseout', function(){tooltip.style("visibility", "hidden");})
      .on('click', function(d, i) {
        //clear rows if active:
        $(".tabSelect").removeClass("active tabSelect");
        var selected = this;
        if (chart.type == "bp-stacked") {
          for (j = 1; j <= count; j++) {
            if ((i+1) == j) {
              $(table.cell(".td" + j).nodes()).addClass("active tabSelect");
            }
          }
        } else {
          d3.selectAll('.bar')
            .attr("class", function(d, i) {
              if (this == selected) {
                if (data[0].var1 !== undefined) {
                  $(table.cell("#td" + (i + 1)).nodes()).addClass("active tabSelect");
                } else {
                  $(table.column(i + 1).nodes()).addClass('active tabSelect');
                }
                return ("bar selected");
              } else {
                return ("bar none");
              }
            });
        }
      });
}

//For one way colored plots and two-way bar plots (where a legend is made on the right)
//LEGEND INTERACTION:
intLegend = function() {
  var legendLayout = document.getElementById('inz-leg-layout.1');
  if (legendLayout) {
    if (data[0].var1 !== undefined) {
      // this is to find the no. of groups for the second variable.
      var group = chart.group[0] + 1;
    } else {
      var group = count;
    };

    //assigning mouse events:
    for (i = 1; i < group; i++) {
        var keyText = document.getElementById('inz-leg-txt-' + i + '.1.1.tspan.1');
        var key = document.getElementById('inz-leg-pt-' + i + '.1.1');

        (function (i) {
            key.addEventListener("mouseover", function () { show(i) }, false);
            key.addEventListener("mouseout", function () { out(i) }, false);
            key.addEventListener("click", function () { subset(i) }, false);

            keyText.addEventListener("mouseover", function () { show(i) }, false);
            keyText.addEventListener("mouseout", function () { out(i) }, false);
            keyText.addEventListener("click", function () { subset(i) }, false);
        })(i)
      }

  subset = function(i) {
    var j = i;
    // clear tables:
    $(".tabSelect").removeClass("active tabSelect");

      d3.selectAll('.bar')
        .attr("class", function(d, i) {
          var i = i + 1;
            if ((i == j || (i%(group-1)) == j || (i%(group-1)) == 0 && j == (group-1))) {
              if(data[0].var1 !== undefined) { // for 2-way tables
                $(table.row((i-1)%(nrow-1)).nodes()).addClass('active tabSelect');
              } else {
                $(table.column(j).nodes()).addClass('active tabSelect');
              }
              return("bar selected");
            } else {
              $(table.row((i-1)%(nrow-1)).nodes()).removeClass('active tabSelect');
              return("bar none");
            }
        });
    }
  };
}

var bars = screenBars(colorMatch);
makeTooltips(bars, data);
if (colorMatch[0] !== true) {
  intLegend();
}

 //Reset Button: attempts to bring plot back to original state
reset = function() {
  d3.selectAll('.bar')
    .attr("class", "bar");
  // reset tables:
  $(".tabSelect").removeClass("active tabSelect");
 };

var plotRegion = document.querySelectorAll('rect[id^="inz-plot-bg"]')[0];
 plotRegion.addEventListener("click", reset, false);

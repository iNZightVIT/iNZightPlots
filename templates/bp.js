/* single bar plots */

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
      }else if (tab[0].var1 !== undefined) {
        td[i - 1].setAttribute('id', 'td' + (((i - (i % ncol)) / ncol + 1) + ((nrow - 1) * (i % ncol - 2))));
    } else if (i < ncol) {
        td[i - 1].setAttribute('class', 'td' + (i - 1));
    } else {
        td[i - 1].setAttribute('class', 'td' + ((i - 2) % ncol + 1));
    }
}

//x-header:
insertXHeader();

if (tab[0].var1 !== undefined) {

  //Finding the sum of countsTab:
    var countsTab = [];
    for (i = 1; i <= cellNo / ncol; i++) {
        var tc = document.getElementById('tc' + i);
        countsTab[i-1] = Number(tc.innerHTML);
  };

  var sum = countsTab.reduce(function(a, b) { return a + b; }, 0);

  //y heading:
  insertYHeader();

  //Summation row:
  var lastRow = table.insertRow(nrow+1);
  lastRow.setAttribute('class', 'totalRow');
  for (i = 1; i <= ncol; i ++) {
    var cell = lastRow.insertCell(i-1);
    cell.id = "totalCell" + (i-1);
    //fill in column totals:
      cell.innerHTML = colCounts[i-1];
  };

  var sumCell = document.getElementById('totalCell' + (ncol-1));
  sumCell.innerHTML = "N = " + sum;

  var totalCell = document.getElementById('totalCell' + (ncol-2));

//insert buttons and conversion functions:
  button("Percentage");
  button("Count");

    //Conversion to percentages:
  changePercentage = function() {

    addClass('ButtonPercentage', 'dark');
    removeClass('ButtonCount', 'dark');

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

  };

  //Conversion to counts:
  changeCount = function() {

    addClass('ButtonCount', 'dark');
    removeClass('ButtonPercentage', 'dark');

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
  };

};



//viewTable button:
  viewTable = document.getElementById('viewTable');
  t = true;

showTable = function() {
  if(t) {
    viewTable.innerHTML = "Hide Table";
    //table.classList.remove('hidden');
    removeClass('table','hidden');
    removeClass('ButtonPercentage','hidden');
    removeClass('ButtonCount', 'hidden');

    t = false;
  } else {
    viewTable.innerHTML = "View Table";
    addClass('table', 'hidden');
    addClass('ButtonPercentage', 'hidden');
    addClass('ButtonCount', 'hidden');

    t = true;
  }
};

// obtaining values from SVG file:
  var Grob = getGrob('bar'),
    number = tab.length,
    count = number + 1;

//if the bar plot is colored - has additional bars and polylines to hide!
if (colorMatch !== null) {
var p = document.getElementsByTagName('polygon');
for (i = 0; i < p.length; i++) {
  if (colorMatch[i+1] == 1) {
   p[i].id = Grob + "." + (i/(number-1));
   p[i].classList.add('bar');
} else {
  p[i].classList.add('hidden');

 }
}
//Hiding polylines:
hideBarLines();
};

//tooltip:
var tooltip = d3.select('body').append('div')
              .attr('class', 'tooltip')
              .attr('id', 'tooltip')
              .style('width', '100px')
              .style('min-height', '40px')
              .style('visibility', 'hidden');


if (colorMatch !== null) {
  var bars = d3.select('svg').selectAll('.bar');
  var plotRegion = document.getElementsByTagName('rect')[2];
} else {
  var panel = document.getElementById(Grob);
  var plotRegion = document.getElementsByTagName('rect')[1];
  var bars = d3.select(panel).selectAll('polygon');
}

if (tab[0].var1 !== undefined) {
  var plotRegion = document.getElementsByTagName('rect')[2];
}

// tooltips:
    bars.data(tab)
    .attr('class', 'bar')
    .on('mouseover', function(d){ var el = d3.select(this);
                                  var coords = el.attr('points');
                                  var small = coords.split(" ")[0];
                                  var sx = Number(small.split(",")[0]);
                                  var coordsxy = coords.split(" ")[2];
                                  var x = Number(coordsxy.split(",")[0]);
                                  var xx = x + (sx-x)/2 - 50;
                                  var y = Number(coordsxy.split(",")[1]) + 60;
                                  //translate to html co-ordinates
                                  var tm = this.getScreenCTM()
                                                   .translate(+ xx, + y);

                                  tooltip.style('visibility', 'visible')
                                              .style("left", (window.pageXOffset + tm.e)  + "px")
                                              .style("top", (window.pageYOffset +tm.f)  + "px"); //position needs fixing
                                  if (tab[0].var1 !== undefined) {
                                    tooltip.html("<span>" + d.var1 + ' ' + d.var2+ "<br/> " + d.pct +
                                    "%</span>" + "<br/>" + d.counts);
                                  } else {
                                    tooltip.html("<span>" + d.varx + "<br/> " + d.pct +
                                    "%</span>" + "<br/>" + d.counts);
                                  }})
    .on('mouseout', function(){tooltip.style("visibility", "hidden");})
    .on('click', function(d, i) {
      for(j = 1; j < count; j ++) { //could refactor this?
        var bar = document.getElementById(Grob + '.' + j);
          //colors:
          var l = bar.getAttribute('fill');
          var lp = l.substring(4, l.length-1);

          if(tab[0].var1 !== undefined) {
            var row = document.getElementById('tr' + ((j-1)%(nrow-1)+1));
            resetTabSelection(row);

            data = document.getElementById('td' + j);
            //it differs due to different formation of tables

            if ((i+1) == j) {
              bar.setAttribute('class', 'bar selected');

              returnTabSelection(lp, data);

            } else {
              bar.setAttribute('class', 'bar none');
              resetTabSelection(data);

            }

          } else {
            data = table.getElementsByClassName('td' + j);

              if ((i+1) == j) {
                bar.setAttribute('class', 'bar selected');

                 for (k = 0; k <= 1; k++) {
                   data[k].style.backgroundColor = "rgba(" + lp + ",0.5)";
                   if (k == 1) {
                     data[k].classList.add('tabSelect');
                   }
               }

              } else {
                bar.setAttribute('class', 'bar none');
                for (k = 0; k <= 1; k++) {
                  resetTabSelection(data[k]);
                  }
                }
              }
          }
    });




// For one way colored plots and two-way bar plots (where a legend is made on the right)
if (tab[0].var1 != undefined) {
  //var percent = JSON.parse(percent); // this is to find the no. of groups for the second variable.
  var group = group +1;
} else {
  var group = count;
};

if (colorMatch != undefined || tab[0].var1 != undefined) {

//LEGEND INTERACTION:
//grabbing keys and text from the legend:
var keys = document.getElementsByTagName('use');
var text = document.getElementsByTagName('text');

//Legend interaction:
for (i = 1; i < group; i ++) {
  key = document.getElementById(keys[i-1].id);
  keyText = document.getElementById(text[i+3].id);
  (function(i){
  keyText.addEventListener("mouseover", function(){show(i)}, false);
  keyText.addEventListener("mouseout", function(){out(i)}, false);
  keyText.addEventListener("click", function(){info(i)}, false);

  key.addEventListener("mouseover", function(){show(i)}, false);
  key.addEventListener("mouseout", function(){out(i)}, false);
  key.addEventListener("click", function(){info(i)}, false);
  }) (i)
};


info = function(i) {
  for (j = 1; j < count; j++) {
      var bar = document.getElementById(Grob + '.' + j);
      var l = bar.getAttribute('fill');
      lp = l.substring(4, l.length-1);

if (tab[0].var1 !== undefined) {
  var row = document.getElementById('tr' + ((j-1)%(nrow-1)+1));
  var td = document.getElementById('td' + j);
    row.classList.remove('tabSelect');
    td.classList.remove('tabSelect');
     td.setAttribute('style', 'inherit');

  if ((j == i || (j%(group-1)) == i || (j%(group-1)) == 0 && i == (group-1))) {
    //for two-way bar plots
    bar.setAttribute('class', 'bar selected');
    returnTabSelection(lp, row);

      } else {
      bar.setAttribute('class', 'bar none');
      resetTabSelection(row);

     }
  } else { //for one way colored bar plots:

      var data = document.getElementsByClassName('td' + j);

if (i == j) {
  bar.setAttribute('class', 'bar selected');

  for (k = 0; k <= 1; k++) {
    returnTabSelection(lp, data[k]);
}
    } else {
    bar.setAttribute('class', 'bar none');

    for (k = 0; k <= 1; k++) {
      resetTabSelection(data[k]);
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

     if(tab[0].var1 != undefined) { // for two way bar plots
       var row = document.getElementById('tr' + ((i-1)%(nrow-1)+1));
        var td = document.getElementById('td' + i);

        resetTabSelection(row);

        td.classList.remove('tabSelect');
        td.setAttribute('style', 'inherit');

     } else { // for one way bar plots
     var data = document.getElementsByClassName('td' + i);
     for (k = 0; k <= 1; k++) {
       resetTabSelection(data[k]);
     }
 }
 }
 };


//plot region reset:
  plotRegion.addEventListener("click", reset, false);

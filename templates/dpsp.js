// dot plots and scatter plots
//initialize DataTable:
var table = $('#table').DataTable({
    "colReorder": true,
    "columnDefs": [
      { //hide rowID column
        "targets": [0],
        "visible": false
      }
    ]
  });

  if (chart.levNames) {
    var levelNo = chart.levNames.length;
  } else {
    var levelNo = 1;
  }

// create form for variable selection:
varForm = function() {
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

  // get column names:
  var ncol = $("#table thead tr th").length;
  var th = document.getElementsByTagName('th');
  for (var i = 0; i <= ncol; i++){
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

varForm();

//drive the viewTable button:
showTable = function() {
  var tableWrapper = $('#table_wrapper');
  tableWrapper.toggleClass('hidden');
};

//find out whether it's a dot or scatter:
if (chart.type == "dot") {
  var Grob = document.querySelectorAll('g[id^="inz-DOTPOINTS."]');
  boxMe(levelNo);
} else {
  var Grob = document.querySelectorAll('g[id^="inz-SCATTERPOINTS."]');
}

// TOOLTIPS:
var tooltip = d3.select('body').append('div')
              .attr('class', 'tooltip')
              .style('width', '100');

//select all points
d3.selectAll(Grob).selectAll('use')
  .attr('class', 'point');

d3.selectAll('.point')
  .data(data)
  .attr('class', 'point')
  .on('mouseover', function (d, i) {
      var g = ' ';
      var names = chart.varNames;
      var selectVar = document.getElementsByTagName('select')[0];
      var sOpt = selectVar.selectedOptions;
      var s = [];
      for (j = 0; j < sOpt.length; j++) {
          s.push(Number(sOpt[j].value));
        }

      for (var j = 0; j < s.length; j++) {
        if (s[j] !== undefined) {
          var w = names[s[j]-1] + ": <span>"
          var t =  data[i][names[s[j]-1]] + "</span> <br>"
       }

       var g = w + t + g;
       var p = s.length;

       if (s.includes(0)) {
         var g = ' ';
         for (var k = 0; k < names.length; k++) {
           var w = names[k] + ": <span>"
           var t =  data[i][names[k]] + "</span> <br>"
         var g = w + t + g;
         var p = names.length;
       }
     }
   }

   return tooltip.style('visibility', 'visible')
                 .style("left", d3.event.pageX - 50 + "px")
                 .style("top", d3.event.pageY - 30 - 15*(p-1) + "px")
                 .html(g);
    })
    .on('mouseout', function () { return tooltip.style("visibility", 'hidden'); })
    .on('click', function (d, i) {
      var selected = this;
        var ind = [];

        d3.selectAll('.point')
            .attr("class", function() {
              if (!d3.event.shiftKey) {
                if (this === selected) {
                  return ("point selected")
                } else {
                  return ("point none");
                }
              } else {
                if (this.getAttribute('class') === "point selected") {
                  return "point selected";
                } else if (this === selected) {
                    return "point selected";
                } else {
                  return "point none";
                }
            }
          });

        //filter table:
        //search for all those that are selected, then extract id num:
        var selected = document.getElementsByClassName('selected');
        for (var j = 0; j < selected.length; j++) {
            var id = selected[j].id;
            if(levelNo > 1) {
              // search for the middle number due to levels:
              var mid = id.substring(0, id.lastIndexOf('.1.'));
              var levNum = Number(mid.substring(mid.lastIndexOf('.') + 1));
              var num = Number(id.substring(id.lastIndexOf('.') + 1));
              // actual index in table:
              var idNum = chart.countsTab[levNum - 1] + num;
            } else {
              var idNum = id.substring(id.lastIndexOf('.') + 1);
            }
            ind.push("^" + idNum + "$");
        }

        table.search('').columns().search('').draw();
        table.columns(0).search(ind.join("|"), true).draw();

    })
    .on('dblclick', function (d, i) { // deselect
      var selected = this;
        var ind = [];
       selected.setAttribute('class', 'point none');

        // update table:
        var selected = document.getElementsByClassName('selected');
          for (var i = 0; i < selected.length; i++) {
              var id = selected[i].id;
              if(levelNo > 1) {
                // search for the middle number due to levels:
                var mid = id.substring(0, id.lastIndexOf('.1.'));
                var levNum = Number(mid.substring(mid.lastIndexOf('.') + 1));
                var num = Number(id.substring(id.lastIndexOf('.') + 1));
                // actual index in table:
                var idNum = chart.countsTab[levNum - 1] + num;
              } else {
                var idNum = id.substring(id.lastIndexOf('.') + 1);
              }
          }
       table.search('').columns().search('').draw();
       table.columns(0).search(ind.join("|"), true).draw();
    });

//link TABLE TO PLOT:
$('#table tbody').on('click', 'tr', function() {
    $(this).toggleClass('active');
    var ind = table.rows('.active')[0];
    d3.selectAll('.point')
      .attr("class", function(d, i) {
        return (ind.includes(i) ? "point selected" : "point none");
      })
    //clear brush
    d3.selectAll('.selection')
      .style("display", "none");
    })

//LEGEND INTERACTION:
// only for single levels
var legendLayout = document.getElementById('inz-leg-layout.1');
if (legendLayout && levelNo == 1) {
  //grabbing keys and text from the legend:
  var colGroupNo = chart.colGroupNo;
  //assigning mouse events:
  for (i = 1; i <= colGroupNo; i++) { //colGroupNo -> colby levels from R (nlevels)
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

    //on click, subsetting occurs:
    subset = function (i) {
      //get the title variable:
      var titleVar = document.getElementById('inz-leg-title.1.1.tspan.1').innerHTML;
      var key = document.getElementById('inz-leg-pt-' + i + '.1.1');
      var keyText = document.getElementById('inz-leg-txt-' + i + '.1.1.tspan.1').innerHTML;
      var names = chart.varNames;
      var count = document.getElementsByClassName('point').length;

      for (j = 1; j <= count; j++) {
          var point = document.getElementById(Grob[0].id + '.' + j);
          if (key.getAttribute('fill') == point.getAttribute('stroke')) {
              point.setAttribute('class', 'point selected');
          } else {
              point.setAttribute('class', 'point none');
          }
      }

      // find column index + filter: (add one for hidden column)
      var colInd = names.indexOf(titleVar) + 1;
      table.search('').columns().search('').draw();
      table.columns(colInd).search("^" + keyText + "$", true).draw();
    }
};

//LINES LEGEND: for trends only + single scatters
// future work: possibly smoothers, LOE (but don't do it if no-one's going to use it.)
var legLineLayout = document.getElementById('inz-leg-lines.1');
var trendInfo = chart.trendInfo;

getEquation = function(t) {
    switch(t) {
                case "linear":
                return "Linear fit:" + "<span> y = " + trendInfo[t][0] + " " +
                       (trendInfo[t][1] > 0 ? ("+" + trendInfo[t][1]) : trendInfo[t][1])
                       + "x " + "</span> <br />" + "R" + "<sup>2</sup> : " + "<span>"
                       + trendInfo[t][2] + "</span>";
                break;

                case "quadratic":
                return "Quadratic fit:" + "<span> y = " + trendInfo[t][0] + " "
                      + (trendInfo[t][1] > 0 ? ("+" + trendInfo[t][1]) : trendInfo[t][1])
                      + "x " + (trendInfo[t][2] > 0 ? ("+" + trendInfo[t][2]) : trendInfo[t][2])
                      + "x<sup>2</sup>" + "</span>" + "<br />"+ "R" + "<sup>2</sup> : " + "<span>"
                      + trendInfo["rank.cor"][0] + "</span>";
                break;

                case "cubic":
                return "Cubic fit: <span> y = " + trendInfo[t][0] + " " +
                        (trendInfo[t][1] > 0 ? ("+" + trendInfo[t][1]) : trendInfo[t][1])
                        + "x " + (trendInfo[t][2] > 0 ? ("+" + trendInfo[t][2]) : trendInfo[t][2])
                        + "x<sup>2</sup> " + (trendInfo[t][3] > 0 ? ("+" + trendInfo[t][3]) : trendInfo[t][3])
                        + "x<sup>3</sup> </span>" + "<br />" +  "R" + "<sup>2</sup> : <span>"
                        + trendInfo["rank.cor"][0] + "</span>";
                break;
        }
}

addLineInteraction = function(t, g, ptip) {
    d3.select(legLineLayout).selectAll("." + t)
    .on('mouseover', function() {
        show(t);
    })
    .on('mouseout', function() {
        out(t);
    })
    .on('click', function() {
        d3.selectAll('.trend')
        .classed('hidden', function() {
          return(this.getAttribute('class').includes(t) == true ? false : true);
      });

      return ptip.style("display", null)
                 .html(g);
        });

    d3.selectAll(".trend." + t)
      .on("mouseover", function() {
          d3.select(".tooltip")
            .style("left", d3.event.pageX - 50 + "px")
            .style("top", d3.event.pageY - 50 + "px")
            .style("visibility", "visible")
            .html(g);
          })
      .on("mouseout", function() {
          d3.select(".tooltip")
            .style("visibility", "hidden");
          })
}

setUpLegend = function(legLineLayout, trendInfo) {
    if (legLineLayout) {
    var lines = d3.select(legLineLayout).selectAll('polyline')
                  .attr('class', function() {
                    var id = this.id;
                    var type = id.substring(id.lastIndexOf("-") + 1, id.indexOf("."));
                    return type;
                  });
    var keys = d3.select(legLineLayout).selectAll('tspan')
                 .attr('class', function() { return this.innerHTML; });

   //insert text into the control panel:
   var ptip = d3.select('#control')
             .append("p")
             .attr('class', 'trend-info')
             .style('display', 'none');

  var ll = legLineLayout.querySelectorAll('tspan');

  //loop over lines
  for (var i = 0; i < ll.length; i++) {
      var t = ll[i].innerHTML;
      var line = document.getElementById('inz-trend-' + t + '.1.1.1.1');
      line.setAttribute('class', t + ' trend');

      var g = getEquation(t);
      addLineInteraction(t, g, ptip);
    }
  }
}

setUpLegend(legLineLayout, trendInfo);

// BRUSH EFFECTS:
var brush = d3.brush()
              .on("start", brushstart)
              .on("brush", brushmove)
              .on("end", brushend);

// find where inz-x-grid is:
var pp = document.getElementById('inz-x-grid.1.1.1').parentNode;
d3.select(pp)
  .insert("g", "g:nth-child(4)")
  .attr("class", "brush")
  .call(brush);

// make handles invisible:
d3.selectAll('.handle')
  .style('opacity', 0);
d3.select('.overlay')
  .attr('width', '100%')
  .attr('height', '100%');
d3.select('.overlay')
  .style('stroke', 'none');

function brushmove() {
  var s = d3.event.selection;
  var x1 = s[0][0];
  var x2 = s[1][0];
  var y1 = s[0][1];
  var y2 = s[1][1];
  var ind = [];

  d3.selectAll('.point')
        .attr('class', function(d, i) {
          var selected = this;
          var x = this.x.baseVal.value;
          var y = this.y.baseVal.value;
          if ((x1 <= x && x <= x2) && (y1 <= y && y <= y2)) {
            return('point selected');
          } else {
            return('point none');
          }
        });

  // find all selected points and return indices:
  var selected = document.getElementsByClassName('selected');
    for (var i = 0; i < selected.length; i++) {
      //calculate idNum:
      var id = selected[i].id;
      if (levelNo > 1) {
      // search for the middle number due to levels:
        var mid = id.substring(0, id.lastIndexOf('.1.'));
        var levNum = Number(mid.substring(mid.lastIndexOf('.') + 1));
        var num = Number(id.substring(id.lastIndexOf('.') + 1));
        // actual index in table:
        var idNum = chart.countsTab[levNum - 1] + num;
      } else {
        var idNum = id.substring(id.lastIndexOf('.') + 1);
      }
        ind.push("^" + idNum + "$");
    }
    //filter table:
    table.columns('').search().columns('').draw();
    table.columns(0).search(ind.join("|"), true).draw();
};

//Reset Button:
reset = function() {
    d3.selectAll('.point')
      .classed("none selected", false);

    // restore table to original state
    table.search('').columns().search('').draw();
    table.rows().nodes().to$().removeClass('active');

    d3.selectAll('.label')
    .classed("hidden", true);

    d3.selectAll('.selection')
      .style("display", "none");

    d3.selectAll('.trend')
      .classed("hidden", false);

    d3.select('.trend-info')
      .classed("hidden", true);

};

// deselection/reset using plotregion double-click:
var plotRegion = document.getElementsByClassName('overlay')[0];
plotRegion.addEventListener("dblclick", reset, false);

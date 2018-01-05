// dot plots and scatter plots
var table = document.getElementById('table'),
    th = document.getElementsByTagName('th'),
    ncol = document.getElementsByTagName('th').length;

d3.selectAll("td")
  .attr("id", function(d, i) { return i + 1; })
  .attr("class", function(d, i) { if ((i + 1) % ncol == 0) {
                                    return ncol;
                                  } else {
                                    return ((i + 1) % ncol);
                                  }
                                });

d3.selectAll("th")
  .attr("class", function(d, i) { return i + 1; });

d3.selectAll("tr")
  .attr("id", function(d, i) { return "tr" + i; });

// create form for variable selection:
function createVariableSelectionForm() {
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

  for (i = 0; i<=ncol; i++){
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

//create form and select options for variable selection/display.
createVariableSelectionForm();

//drive the viewTable button:
  var t = true;
showTable = function() {
  var viewTable = document.getElementById('viewTable');
  if(t) {
    viewTable.innerHTML = "Hide Table";
    d3.select('table')
      .classed('hidden', false);
    t = false;
  } else {
    viewTable.innerHTML = "View Table";
    d3.select('table')
      .classed('hidden', true);
    t = true;
  }
};

//find out whether it's a dot or scatter:
// if a box plot exists:
var boxElements = document.getElementById('inz-box.1.1.1.1');
if (boxElements !== null) {
  // for dotplots only...
  var Grob = "inz-DOTPOINTS.1.1.1.1",
      lastLine = "inz-box-line.1.1.1.1";
  boxMe(lastLine, boxElements);
} else {
  var Grob = "inz-SCATTERPOINTS.1.1.1";
}

var panel = document.getElementById(Grob);
var count = panel.childElementCount;

//make  tooltip:
var tooltip = d3.select('body').append('div')
              .attr('class', 'tooltip')
              .style('width', '100');

d3.select(panel).selectAll('use')
  .data(data)
  .attr('class', 'point')
  .on('mouseover', function (d, i) {
      var g = ' ';
      var names = chart.varNames;
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
      for (j = 1; j <= count; j++) {
          var point = document.getElementById(Grob + "." + j);
          var l = point.getAttribute('stroke');
          if (l == "none") {
            var l = point.getAttribute('fill');
          }
          var lp = l.substring(l.lastIndexOf("("), l.lastIndexOf(")"));
          var dataRow = document.getElementById('tr' + j);
          if (point.getAttribute('class') !== "point selected") {
              if ((i+1) == j) {
                  point.setAttribute('class', 'point selected');
                  returnRowSelection(lp, dataRow);
              } else {
                  point.setAttribute('class', 'point none');
                  omitRowSelection(dataRow);
              }
            }
        }

        d3.selectAll('.boxData')
          .classed('hidden', true);
    })

    .on('dblclick', function (d, i) { // deselect
        for (j = 1; j <= count; j++) {
        var point = document.getElementById(Grob + "." + j);
        var dataRow = document.getElementById('tr' + j);
        if ((i+1) == j) {
            point.setAttribute('class', 'point none');
            omitRowSelection(dataRow);
        }
      }
    });

//LEGEND INTERACTION:
// TODO: change to d3 usage.
var legendLayout = document.getElementById('inz-leg-layout.1');

if (legendLayout) { // if there is a legend, colGroupNo should be a value
    //grabbing keys and text from the legend:
    var keys = document.getElementsByTagName('use');
    var text = document.getElementsByTagName('text');
    var colGroupNo = chart.colGroupNo;

    //assigning mouse events:
    for (i = 1; i <= colGroupNo; i++) { //colGroupNo -> colby levels from R (nlevels)
        var key = document.getElementById(keys[i - 1].id),
            keyText = document.getElementById(text[i + 3].id);
        if (Grob == "inz-DOTPOINTS.1.1.1.1") { // for dot plots - legend text differs
            var keyText = document.getElementById(text[i + 2].id);
        }
        (function (i) {
            key.addEventListener("mouseover", function () { show(i) }, false);
            key.addEventListener("mouseout", function () { out(i) }, false);
            key.addEventListener("click", function () { subset(i) }, false);

            keyText.addEventListener("mouseover", function () { show(i) }, false);
            keyText.addEventListener("mouseout", function () { out(i) }, false);
            keyText.addEventListener("click", function () { subset(i) }, false);
        })(i)
    };

    //on click, subsetting occurs:
    subset = function (i) {
        for (j = 1; j <= count; j++) {
            var point = document.getElementById(Grob + '.' + j);
            var key = document.getElementById(keys[i - 1].id);
            var gLabel = document.getElementById('gLabel' + j);
            var dataRow = document.getElementById('tr' + j);

            if (key.getAttribute('fill') == point.getAttribute('stroke')) {
                point.setAttribute('class', 'point selected');
                returnRowSelection('0,0,0', dataRow);

            } else {
                point.setAttribute('class', 'point none');
                omitRowSelection(dataRow);
            }
        }
    }
};

var brush = d3.brush()
              .on("brush", brushmove)
              .on("end", brushend);

// attaching brush to correct positions:
if (chart.type == "dot") {
  var pp = panel.parentNode.parentNode,
      insertBrush = ":first-child";
} else {
  var pp = panel.parentNode,
      insertBrush = "g:nth-child(4)";
}

d3.select(pp)
  .insert("g", insertBrush)
  .attr("class", "brush")
  .call(brush);

// make handles invisible:
d3.selectAll('.handle')
  .style('opacity', 0);

function brushmove() {
  var s = d3.event.selection;
  var x1 = s[0][0];
  var x2 = s[1][0];
  var y1 = s[0][1];
  var y2 = s[1][1];

  for (i =1; i <= count; i++) {
    var point = document.getElementById(Grob + '.' + i);
    var x = point.x.baseVal.value;
    var y = point.y.baseVal.value;
    var dataRow = document.getElementById('tr' + i);

    //points that lie within the  boundary box drawn:
    if ((x1 <= x && x <= x2) && (y1 <= y && y <= y2)) {
      var l = point.getAttribute('stroke');
      if (l == "none") {
        var l = point.getAttribute('fill');
      }
      var lp = l.substring(l.lastIndexOf("("), l.lastIndexOf(")"));
      returnRowSelection(lp, dataRow);
      point.setAttribute('class', 'point selected');
    } else {
      point.setAttribute('class', 'point none');
      omitRowSelection(dataRow);
    }
  }
};

//Reset Button:
reset = function() {
    d3.selectAll('.point')
      .classed("none selected", false);

    d3.selectAll('tr')
      .classed("hidden rowSelect", false)
      .style("background-color", "white");

    d3.selectAll('.boxData')
      .classed("hidden", true);

    d3.selectAll('.selection')
      .style("display", "none");
};

// deselection/reset using plotregion double-click:
var plotRegion = document.getElementsByClassName('overlay')[0];
plotRegion.addEventListener("dblclick", reset, false);

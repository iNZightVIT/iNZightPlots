// dot plots and scatter plots

var table = document.getElementById('table'),
nrow = document.getElementById('table').rows.length,
th = document.getElementsByTagName('th'),
ncol = document.getElementsByTagName('th').length,
td = document.getElementsByTagName('td'),
cellNo = td.length;

for (i = 1; i <= cellNo; i ++) {
  td[i-1].setAttribute('id', i);
  td[i-1].setAttribute('class', i%ncol);
  if (i%ncol == 0) {
    td[i-1].setAttribute('class', ncol);
  }
};

for (j = 1; j <= ncol; j++) {
  th = document.getElementsByTagName('th');
  th[j-1].setAttribute('class', j);
};

//no. of rows in table
for (i = 1; i < nrow; i ++) {
  var tr = document.getElementsByTagName('tr');
  tr[i].setAttribute('id', 'tr' + i);
};

//create form and select options for variable selection/display.
createVariableSelectionForm();

//drive the viewTable button:
  var viewTable = document.getElementById('viewTable');
  t = true;
showTable = function() {
  if(t) {
    viewTable.innerHTML = "Hide Table";
    table.classList.remove('hidden');
    t = false;
  } else {
    viewTable.innerHTML = "View Table";
    table.classList.add('hidden');
    t = true
  }
};

//find out whether it's a dot or scatter:

if (boxData != undefined) {
  // for dotplots only...
  var Grob = "DOTPOINTS.1";

  var lastLine = getMinMaxLinesId();
  getBoxes("dotplot");

  boxLabelSet(0, 1, 0,'LQ');
  boxLabelSet(1, 2, 2, 'UQ');
  boxLabelSet(1, 0, 1, 'Median');
  boxLabelSet(1, 0, 3, 'Min');
  boxLabelSet(2, 1, 4, 'Max');

  //Box Plot interactions:
  var box = document.getElementsByClassName('box');
  var boxData = document.getElementsByClassName('boxData');

  //setting interactions and colors for box plot:
  for (i = 0; i < box.length; i++) {
    box[i].addEventListener('mouseover', fillBox, false);
    box[i].addEventListener('mouseout', normalBox, false);
    box[i].addEventListener('click', showBox, false);
  }

} else {
  var Grob = "SCATTERPOINTS.1";
}

var panel = document.getElementById(Grob);
var count = panel.childElementCount;

//make  tooltip:
var tooltip = d3.select('body').append('div')
              .attr('class', 'tooltip')
              .attr('id', 'hello')
              .style('width', '100');

d3.select(panel).selectAll('use')
    .data(tableData)
    .attr('class', 'point')
    .on('mouseover', function (d, i) {
      var g = ' ';
      var sOpt = selectVar.selectedOptions;
      var s = [];
      for (j = 0; j < sOpt.length; j++) {
          s.push(sOpt[j].value);
        }

      for (var j = 0; j < s.length; j++) {
        if (s[j] !== undefined) {
          var w = names[s[j]-1] + ": <span>"
          var t =  tableData[i][names[s[j]-1]] + "</span> <br>"
       }
       var g = w + t + g;
       var p = s.length;

       if (s[j].indexOf('0') >= 0) {
         var g = ' ';
         for (var k = 0; k < names.length; k++) {
           var w = names[k] + ": <span>"
           var t =  tableData[i][names[k]] + "</span> <br>"
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
            var lp = l.substring(l.lastIndexOf("("), l.lastIndexOf(")"));

            var dataRow = document.getElementById('tr' + j);

            if (point.getAttribute('class') != "point selected") {
                if ((i+1) == j) {
                    point.setAttribute('class', 'point selected');
                    returnRowSelection(lp, dataRow);

                } else {
                    point.setAttribute('class', 'point none');
                    omitRowSelection(dataRow);
                }
            }
        }
        if (boxData != undefined) {
            boxData = document.getElementsByClassName('boxData');
            hideBox();
        }
    })// connect to table + isolate
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

if (colGroupNo != (0 || undefined)) { // if there is a legend, colGroupNo should be a value
    //grabbing keys and text from the legend:
    var keys = document.getElementsByTagName('use');
    var text = document.getElementsByTagName('text');

    //assigning mouse events:
    for (i = 1; i <= colGroupNo; i++) { //colGroupNo -> colby levels from R (nlevels)
        var key = document.getElementById(keys[i - 1].id),
            keyText = document.getElementById(text[i + 3].id);
        if (Grob == "DOTPOINTS.1") { // for dot plots - legend text differs
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


//obtaining svg region:
var svg = document.getElementsByTagName('svg')[0];
svg.setAttribute('draggable', 'false');

//create invisible selection box that is enabled when dragging occurs:
d3.select(panel).append('polygon')
    .attr('id', 'selectRect')
    .attr('class', 'selectRect');

var evt = window.event;
var zoomBox = {};

//Attach mouse events:
svg.setAttribute('onmouseup', 'MouseUp(evt)');
svg.setAttribute('onmousemove', 'MouseDrag(evt)');
svg.setAttribute('onmousedown', 'MouseDown(evt)');

MouseDrag = function (evt) {
  if(zoomBox["isDrawing"]) {
    var pt = svg.createSVGPoint();
    pt.x = evt.pageX;
    pt.y = evt.pageY;
    var pt = pt.matrixTransform(svg.getScreenCTM().inverse());
      zoomBox["endX"] = pt.x;
      zoomBox["endY"] = pt.y;

      //Because the y-axis is inverted in the plot - need to invert the scale
       tVal = document.getElementsByTagName('g')[0].getAttribute('transform').substring(13, 16);
      var selectRect = document.getElementById('selectRect');

       // for rectangles with positive height, positive width
      if(zoomBox["startX"] < zoomBox["endX"]) {
      var x1 = zoomBox["startX"];
      var x2 = zoomBox["endX"];
    } else {
      var x1 = zoomBox["endX"];
      var x2 = zoomBox["startX"];
    }

    // for rectangles with opposite directions ('negative' widths, heights)
    if (zoomBox["startY"] < zoomBox["endY"]) {
      var y1 = tVal - zoomBox["startY"] - (zoomBox["endY"]-zoomBox["startY"]);
      var y2 = y1 + (zoomBox["endY"]-zoomBox["startY"]);
    } else {
      var y1 = tVal - zoomBox["endY"] - (zoomBox["startY"]-zoomBox["endY"]);
      var y2 = y1 + (zoomBox["startY"]-zoomBox["endY"]);
    }
      //selectRect.setAttribute('transform', 'translate(0, 668) scale(-1, 1)');
      selectRect.setAttribute('points', x1 + ',' + y1 + " " + x1 + ',' + y2 + ' '
                                        + x2 + ',' + y2 + ' ' + x2 + ',' + y1);

        for (i = 1; i <= count; i++) {
            var point = document.getElementById(Grob + '.' + i);
            var dataRow = document.getElementById('tr' + i);

            var x = point.x.baseVal.value;
            var y = point.y.baseVal.value;

            //points that lie within the  boundary box drawn:
            if ((x1 <= x && x <= x2) && (y1 <= y && y <= y2)) {
                point.setAttribute('class', ' point selected');
                l = point.getAttribute('stroke');
                lp = l.substring(l.lastIndexOf("("), l.lastIndexOf(")"));
                returnRowSelection(lp, dataRow);

            } else {
                point.setAttribute('class', 'point none');
                omitRowSelection(dataRow);
            }
        }
    }
};

//Reset Button:
reset = function() {
    for (i = 1; i <= count; i++) {
        var point = document.getElementById(Grob + "." + i);
        point.setAttribute('class', 'point');

        var dataRow = document.getElementById('tr' + i);
        resetRowSelection(dataRow);

    }
    // reset selection rectangle
    var selectRect = document.getElementById('selectRect');
    if (selectRect != undefined) {
        selectRect.setAttribute('points', '0,0');
    }

};

// deselection/reset using plotregion double-click:
var plotRegion = document.getElementsByTagName('rect')[1];
plotRegion.addEventListener("dblclick", reset, false);

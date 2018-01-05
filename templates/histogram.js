// for histograms
var table = document.getElementById('table');
    nrow = table.rows.length, //no. of rows in table
    td = document.getElementsByTagName('td'),
    tr = document.getElementsByTagName('tr'),
    totalRow = tr[nrow-1],
    cellNo = td.length;

d3.selectAll('td')
  .attr('id', function(d, i) { return("td" + i); })

d3.selectAll('tr')
  .attr('id', function(d, i) { return ("tr" + i); })

totalRow.setAttribute('class', 'totalRow');

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

var svg = document.getElementsByTagName('svg')[0],
    count = data.length,
    Grob = getGrob('hist')
    panel = document.getElementById(Grob);

var tooltip = d3.select('body').append('div')
              .attr('class', 'tooltip')
              .attr('id', 'tooltip')
              .style('width', '200')
              .style('height', '50');

d3.select(panel).selectAll('polygon')
    .data(data)
    .attr('class', 'histBar')
    .on('mouseover', function(d){tooltip.style('visibility', 'visible')
                                              .style("left", d3.event.pageX - 40 + "px")
                                              .style("top", d3.event.pageY - 55 + "px")
                                              .html("Class range: <br> <span>" + d.lower + " - " + d.upper +
                                              " </span>" + "<br> N = <span>" +
                                              d.counts + ", " + d.pct + "% </span>");})
    .on('mouseout', function(){tooltip.style("visibility", "hidden");})
    .on('click', function(d, i) {

      var selected = this;
      var l = selected.getAttribute('fill');
      var lp = l.substring(l.lastIndexOf("("), l.lastIndexOf(")"));
      var col = "rgba" + lp + ", 0.25)";

      d3.select(panel).selectAll('polygon')
        .attr("class", function() {
          return (this === selected ? "histBar selected" : "histBar none");
        })

        var dataRow = document.getElementById('tr' + (i + 1));
        d3.selectAll('tr')
          .style('background-color', function() {
            return(this === dataRow ? col : "white");
          })
          .attr("class", function(d, i) {
            if (i === 0) {
              return "header"
            } else {
              return(this === dataRow ? "rowSelect" : "hidden");
            }
          });
      // hide box
      d3.selectAll('.boxData')
        .classed('hidden', true);
      d3.select("#tt").style('visibility', 'hidden');
    });

/* -------------------------------------------------------------
                  Box plot properties and interactions:
---------------------------------------------------------------- */
//identify lines and box plot:
var lastLine = "inz-box-line.1.1.1.1";
var boxElements = document.getElementById('inz-box.1.1.1.1');
boxMe(lastLine, boxElements);

 // create another tooltip for selection box:
 var tt = d3.select('#control').append('div')
            .attr('class', 'tooltip')
            .attr('id', 'tt')
            .style('width', '150')
            .style('padding', '5px')
            .style('margin-top', '5px')
            .style('height', '35');

 //create invisible selection box that is enabled when dragging occurs:
 var brush = d3.brush()
              .on("start", brushstart)
              .on("brush", brushmove)
              .on("end", brushend);

 var pp = panel.parentNode.parentNode;
 d3.select(pp)
  .insert("g", "g:first-child") //"g" ":first-child"
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

    // information to extract:
    var n = chart.n;
    var tab = chart.data;
    var groupN = [];
    var intRange = [];

    for (i = 1; i <= count; i++) {
    var bar = document.getElementById(Grob + '.' + i);
    var dataRow = document.getElementById('tr' + i);

    //obtain end points of the bar
    //TODO: maybe fix this so you don't have to select the bottom?:
    var coords = bar.getAttribute('points').split(" ");
    var bottomEdge = coords[0].split(',');
    var topEdge =  coords[3].split(',');
    var bx = bottomEdge[0];
    var by = bottomEdge[1];
    var tx = topEdge[0];
    var ty = topEdge[1];

      if (bar.getAttribute('visibility') == 'hidden') {
        // those that are hidden, remain hidden
        bar.classList.add('hidden');
      } else {
        //points that lie within the  boundary box drawn:
        if(((x1 <= bx && bx <= x2) && (x1 <= tx && tx <= x2)) && ((y1 <= by && by <= y2) && (y1 <= ty && ty <= y2))) {
          bar.setAttribute('class', ' histBar selected');
          l = bar.getAttribute('fill');
          lp = l.substring(l.lastIndexOf("("), l.lastIndexOf(")"));
          returnRowSelection(lp, dataRow);
          // store frequency from selected
          groupN.push(tab[i - 1].counts);
          intRange.push(tab[i - 1].lower, tab[i - 1].upper);
        } else {
          bar.setAttribute('class', 'histBar none');
          omitRowSelection(dataRow);
       }
     }

     //summation of array:
     var sum = groupN.reduce(function(a, b) { return a + b; }, 0);

     d3.select('.totalRow')
       .classed('hidden', false);
     var total = td[td.length-1];
     total.innerHTML = sum;

    // report a proportion + interval number:
    var nProp = (sum/n*100).toFixed(2) + "%";
    var intervalNo = document.getElementsByClassName('selected').length;

    // create another  tooltip for selection box:
    d3.select("#tt").style('visibility', 'visible')
                    .html("Interval Range: <span>" + intRange[0] + " - " + intRange[intRange.length-1] +
                          "</span> <br> Frequency: <span>" + sum +  "," + nProp + "</span> <br> No. of intervals: <span>" +
                          intervalNo + "</span>");
   }
};

//Reset Button:
reset = function() {
    d3.select(panel).selectAll('.histBar') //polygon
      .attr('class', 'histBar');

    d3.select('.totalRow')
      .classed('hidden', false);

    d3.select('#tt')
      .classed('hidden', true);

    d3.selectAll('tr')
      .classed('hidden rowSelect', false)
      .style('background-color', 'white');

    //reset total to n
    var total = td[td.length-1];
    total.innerHTML = chart.n;

    //remove box:
    d3.selectAll(".selection")
      .style("display", "none");

    //hide boxplot data:
    d3.selectAll('.boxData')
      .classed('hidden', true);

    var viewTable = document.getElementById('viewTable');
    viewTable.innerHTML = "View Table";
    table.classList.add('hidden');
    t = true;
 };

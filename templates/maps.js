// ggplot2 maps for iNZightMaps
var svg = d3.select('svg')
            .attr('width', null)
            .attr('height', null);

// initialize data table
var table = $('#table').DataTable({
      "colReorder": true,
      "columnDefs": [
        { //hide rowID column
          "targets": [0],
          "visible": false
        }
      ]
  });

filterMissing = function(chart) {
  // ids to take into account of missing values:
  var data = chart.data;
  var names = chart.names;
  var arr = [];

  for (i = 0; i < data.length; i++) {
    if (data[i][names[2]]) {
      arr.push(i);
    } else {
      continue;
    }
  }
  return (arr);
}

// Display and hide the table
showTable = function() {
    var tableWrapper = $('#table_wrapper');
    tableWrapper.toggleClass('hidden');
};

// assign and find correct elements:
  var data = chart.data,
      names = chart.names,
      pathElements = document.querySelectorAll('g[id^="grill.gTree"]~g[id^="GRID.pathgrob"] path'),
      sparkElements = document.querySelectorAll('g[id^="GRID.gTree"] path'),
      regions = d3.selectAll(pathElements)
                  .attr('class', 'region')
                  .attr('id', function(d, i) { return(data[i][names[0]] + "." + i); }),
      sparkRegions = d3.selectAll(sparkElements)
                       .attr('class', 'sparkRegion')
                       .attr('id', function(d, i) { return("sparkRegion." + i)}),
      centroids = d3.select('svg').selectAll('use')
                    .attr('class', 'centroid'),
      tooltip = d3.select('body').append('div')
                  .attr('class', 'tooltip')
                  .style('width', '100'),
     defaultColours = ["#101f33", "#162e47", "#1b3c5e", "#234c78", "#2b5f94",
                       "#3572b2", "#3c89d3", "#46a0f7"];

// return column names:
var colNames = document.getElementsByTagName('th');
var cnames = [];
for (i = 0; i < colNames.length; i++) {
  cnames.push(colNames[i].innerHTML.trim());
}

  var arr = filterMissing(chart);

  d3.selectAll(".centroid").attr('id', function(d, i) {
                    return("centroid." + arr[i]);
                  });

  d3.selectAll(".sparkRegion").attr('id', function(d, i) {
                    return("sparkRegion." + arr[i]);
                  });

//make tooltips!
makeTooltips = function(chart) {

  var data = chart.data;

    d3.selectAll(".region")
      .data(data)
      .on("mouseover", function(d, i) {
         var selected = d3.select(this);
        // make things go light and dark:
        selected.classed("selectRegion", true);

        var g = " ";
        var p = names.length;

        for (var j = 0; j < p; j++) {
            var w = names[j] + ": <span>";
            var t = data[i][names[j]] + "</span> <br />";
            var g = w + t + g;
        }

        // region + specific variables:
        return tooltip.style('visibility', 'visible')
                      .style('left', d3.event.pageX - 50 + "px")
                      .style('top', d3.event.pageY - 50 - 15 * (p - 1) + "px")
                      .html(g);
    })
    .on('mouseout', function() {
        var selected = d3.select(this);
        selected.classed('selectRegion', false);
        tooltip.style('visibility', 'hidden');
    })
    .on('click', function(d, i) {
      var selected = this;
      var ind = [];
      var dd = [];

      d3.selectAll('.region')
        .attr("class", function(d, i) {
          //if the shift key is down, accumulate:
          if(d3.event.shiftKey) {
                if(this.getAttribute('class') === "region selected") {
                    return "region selected";
                    ind.push(i);
                } else if (this === selected) {
                    return "region selected";
                    ind.push(i);
                } else {
                    return "region none";
                }
        } else {
            if(this === selected) {
                return "region selected";
                ind.push(i);
            } else {
                return "region none";
            }
        }
        })

        var j = i;

        // for point maps:
        d3.selectAll('.centroid')
          .attr("class", function(d, i) {
            if (!d3.event.shiftKey) {
              if (this.getAttribute('id') == ("centroid." + j)) {
                return ("centroid selected")
              } else {
                return ("centroid none");
              }
            } else {
              if (this.getAttribute("class") === "centroid selected") {
                return ("centroid selected");
              } else if (this.getAttribute('id') == ("centroid." + j)) {
                return ("centroid selected");
              } else {
                return ("centroid none");
              }
            }
          })

          // fade sparkregions:
          sparkRegions.attr('class', function(d, i) {
            if (!d3.event.shiftKey) {
              if (this.getAttribute('id') == ("sparkRegion." + j)) {
                  return "sparkRegion selected";
              } else {
                  return "sparkRegion none";
              }
            } else {
              if (this.getAttribute("class") === "sparkRegion selected") {
                return ("sparkRegion selected");
              } else if (this.getAttribute('id') == ("sparkRegion." + j)) {
                return ("sparkRegion selected");
              } else {
                return ("sparkRegion none");
              }
            }
          })

        if (chart.multi[0] == false && chart.type !== "region") {
          table.search('').columns().search('').draw();
        }

        // find column number:
        var colNum = cnames.findIndex(cnames => cnames === names[0]);

        // find selected regions:
        var regionsSelected = document.getElementsByClassName('region selected');
          for (var i = 0; i < regionsSelected.length; i++) {
              var id = regionsSelected[i].id;
              var regionName = id.substring(0, id.lastIndexOf('.'));
              dd.push(regionName);
              ind.push("^" + regionName + "$");
          }

        table.columns(colNum + 1).search(ind.join("|"), true, false).draw();

        if (chart.type == "sparklines") {
            var group = chart.names[0];
            // update line chart according to specific variable
            changeVar();
          }
    })
};

/**
* filter data to specific region
* @param data full data set exported from R (timeData)
* @param group variable that stores the region names
* @param region the region that is selected
* @return returns filtered data set
*/
filterData = function(data, group, region) {
    var ava = [];
    //could possibly use filter?
    for (i = 0; i < data.length; i++) { // to take into account of missings
        if (data[i][group] == region || data[i][group] == undefined) {
            ava.push(data[i]);
        }
    }
    return ava;
}

/**
 * link table to plot
 * @param {String} type chart type (generally 'region')
 * @return links table to plot (when you click, it highlights specific region)
*/
tableToPlot = function(type) {
  //link TABLE TO PLOT:
  $('#table tbody').on('click', 'tr', function() {
    $(this).toggleClass('active');
    var r = table.rows('.active').data();
    var ind = [];
    // go by region rather than by index:
    var colNum = cnames.findIndex(cnames => cnames === names[0]);
    var regions = [];
    for (i = 0; i < r.length; i++) {
      regions.push(r[i][colNum + 1]);
    }

    d3.selectAll("." + type)
      .attr("class", function() {
        var id = this.id;
        var rr = id.substring(0, id.lastIndexOf("."))
        if (regions.includes(rr)) {
          ind.push(+id.substring(id.lastIndexOf(".") + 1));
        }
        return (regions.includes(rr) ? type + " selected" : type + " none");
      });

    // for centroids:
    if (chart.type == "region") {
        return;
      } else if (chart.type == "point") {
        var p = "centroid";
      } else {
        var p = "sparkRegion";
      }

      var arr = filterMissing(chart);
      for (i = 0; i < arr.length; i++) {
         var el = document.getElementById(p + "." + arr[i]);
        if (ind.includes(arr[i])) {
          el.setAttribute('class', p + " selected");
        } else {
          el.setAttribute('class', p + " none");
        }
      }
  })
}

// reset the entire plot + table
reset = function() {

  //clear map
  d3.selectAll('.region')
    .classed('selected none', false);

  d3.selectAll('.sparkRegion')
    .classed('.sparkRegion none', false)
    .classed('hidden', false);

  d3.selectAll('.centroid')
    .classed('.centroid none', false);

  // clear table
  table.search('').columns().search('').draw();
  table.rows().nodes().to$().removeClass('active');

  if (chart.type == "region" && chart.multi[0] == true) {
    // screen for certain year:
    setTable();
  }

  d3.select('.spark-div')
    .classed('hidden', true);

  //reset zooming:
  d3.select('.background')
    .attr('transform', null);

}

/* set up the line chart that links to specific spark line
 * @param chart data exported from R
 * @return sets up the chart using d3
*/
sparkPlot = function(chart) {
    // get x and y variables
    var xvar = chart.names[1];
    var yvar = chart.names[2];
    var timeData = chart.timeData;

    // TODO: responsive?
    var sparkContainer = d3.select('.tbl-div').insert('div', ":first-child")
                       .attr('class', 'spark-div')
                       .classed('hidden', true);

    var margin = {top: 60, right: 50, bottom: 40, left: 50};

    var spark = d3.select('.spark-div').append('svg')
            .attr('class', 'sparkPlot')
            .attr('viewBox', "0 0 600 400")
            .attr('width', '100%')
            .attr("preserveAspectRatio", "xMidYMid meet");

    var g = spark.append('g')
                 .attr('class', 'spark-group')
                 .attr('transform', 'translate(' + margin.left + ', ' + margin.top + ')');

    var width = $('.tbl-div').width() - margin.left - margin.right;
    var height = 300 - margin.bottom;

    // create x and y scales:
    xScale = d3.scaleLinear()
               .range([0, width]);

    yScale = d3.scaleLinear()
               .range([height, 0]);

    xScale.domain(d3.extent(timeData, function(d) { return d[xvar]; }));

    // xaxis:
    g.append("g")
     .attr("class", "spark-xaxis")
     .attr("transform", "translate(0," + (height) + ")")
     .call(d3.axisBottom(xScale)
     .tickFormat(d3.format("d")));

    //yaxis:
    g.append("g")
     .attr("class", "spark-yaxis")
     .call(d3.axisLeft(yScale));

    // for lines:
    g.append("g")
     .attr("class", "line-group")
     .append("path");

    //add axis labels:
    var xlab = spark.append("text")
                    .attr("transform", "translate(" + width/1.75 + "," + (height + margin.bottom + margin.top) + ")")
                    .style("text-anchor", "center")
                    .text(xvar);

    var ylab = spark.append("text")
                    .attr("class", "y-lab")
                    .attr("transform", "translate(0," + (margin.top - 10) +")")
                    .style("text-anchor", "center")
                    .text(yvar);

    // add a title for the region:
    var title = spark.append("text")
                     .attr("transform", "translate(" + width/2 + "," + (margin.top - 40) + ")")
                     .attr("class", "spark-title")
                     .attr("font-size", "1.5em")
                     .style("text-anchor", "center");

    // add region text labels:
    var regionLab =  d3.select('.spark-group').append('g')
                       .attr("class", "spark-reglab")
                       .append("text")
                       .attr("class", "region-lab");

    // add focus lines - adapted from: https://bl.ocks.org/alandunning/cfb7dcd7951826b9eacd54f0647f48d3
    var lineTool = g.append('g')
                    .attr('class', 'lineTool')
                    .style('display', 'none');

    lineTool.append("line")
            .attr("class", "x-line")
            .attr("y1", 0)
            .attr("y2", height)
            .attr('stroke-dasharray', '0.5, 1');

    lineTool.append("circle")
            .attr("class", "follow-path");

    lineTool.append("text")
            .attr("class", "follow-text");

    spark.append("rect")
         .attr("transform", "translate(" + margin.left + "," + margin.top + ")")
         .attr("class", "spark-overlay")
         .attr("width", width)
         .attr("height", height)
         .style('fill', 'transparent')
         .on("mouseover", function() { lineTool.style("display", null); })
         .on("mouseout", function() { lineTool.style("display", "none"); });

}

// set up a new legend for switching between variables
// for now refers to a continuous colour scale
setLegend = function() {
    //add new scale:
    var layout = document.querySelectorAll("g[id='layout.2']");

    var newScale = d3.selectAll(layout)
                     .append('g')
                     .attr('id', 'new-scale')
                     .classed('hidden', true);

    var defs = newScale.append("defs");

    var linearGradient = defs.append("linearGradient")
                             .attr("id", "linear-gradient");

        linearGradient.attr("x1", "0%")
                      .attr("x2", "0")
                      .attr("y1", "0%")
                      .attr("y2", "100%");

    // get tranformation coordinates: for numerics
    var sRect = document.querySelectorAll('g[id^="bar.4-2-4-2.1.1"]')[0].getAttribute('transform');
    // get coordinates:
    var coords = sRect.substring(sRect.lastIndexOf("(") + 1, sRect.lastIndexOf(")")).split(",");
    var w = 15;
    var h = 100;

    // append rect scale:
    newScale.append('rect')
      .attr('width', w)
      .attr('height', h)
      .attr('transform', 'translate(' + coords[0] + "," + (coords[1] - h) + ")")
      .attr('stroke', 'none')
      .attr("fill", "url(#linear-gradient)")
      .attr('fill-opacity', 1);

  var leg = newScale.append('g')
          .attr("class", "leg-axis")
          .attr("fill", "black")
          .attr("fill-opacity", 1)
          .attr("stroke", "none")
          .attr("transform", "translate(" + (+coords[0] + w) + "," + coords[1] + ") scale(1, -1)");
           //flip, cause gridSVG flips

  //add an axis title:
  var legTitle = newScale.append("text")
                         .attr("class", "leg-title")
                         .attr("fill", "black")
                         .attr("fill-opacity", 1)
                         .attr("transform", "translate(" + coords[0] + "," +  (+coords[1] + 10) + ") scale(1, -1)");
}

// for updating fills on regional maps (variable change)
updateFills = function() {

  var yvar = d3.select('.control-var').property('value'),
      data = chart.data,
      pal = (chart.palette) ? chart.palette : defaultColours;

  if (chart.multi[0] == true) {
    var dd = chart.timeData;
  } else {
    var dd = data;
  }

  //hide original scale:
  var ss = document.querySelectorAll('g[id^="guide-box"]');
      d3.selectAll(ss)
        .classed('hidden', true);

  // divide domain according to number of colours supplied.
   var dval = [];
   var min = d3.min(dd, function(d) { return d[yvar]; });
   var max = d3.max(dd, function(d) { return d[yvar]; });

   for (i = 0; i < pal.length; i++) {
      dval.push(min + ((max - min)/(pal.length - 1) * i));
   }

//colour scale goes by whatever pal is.
   colorScale = d3.scaleLinear()
                     .range(pal)
                     .domain(dval);

  //update linear gradient:
  // code adapted from: https://www.visualcinnamon.com/2016/05/smooth-color-legend-d3-svg-gradient.html
  var linearGradient = d3.select('#linear-gradient');

  //remove and reset all colours on the scale:
  linearGradient.selectAll("stop").remove();

  var stops = linearGradient.selectAll("stop")
                            .data(dval)
                            .enter().append("stop")
                            .attr("offset", function(d, i) { return(i/dval.length * 100 + "%"); })
                            .attr("stop-color", function(d) { return colorScale(d); });

  //update axes and update legend:
  var legAxis = d3.scaleLinear()
                  .range([100, 0])
                  .domain([min, max]);

  var leg = d3.select('.leg-axis')
              .call(d3.axisLeft(legAxis)
              .ticks(4)
              .tickFormat(d3.format("d")));

// attempt to mimic 'ggplot2' legend:
  leg.selectAll('path')
     .attr('stroke', 'none');

  leg.selectAll('line')
      .attr('stroke', 'white');

  leg.selectAll("text")
     .attr('x', 1)
     .attr('font-size', 7)
     .style("text-anchor", "start");

  d3.select('#new-scale')
    .classed('hidden', false);

  // update legend title:
  d3.select('.leg-title')
    .text(yvar);

  // update regions:
  d3.selectAll('.region')
    .data(data)
    .transition()
    .duration(500)
    .attr("fill", function(d) {
      return(d[yvar] ? colorScale(d[yvar]) : colorScale(0));
    });

  // reassign yvar assessed to update tooltip:
  chart.names[chart.names.length - 1] = yvar;

}

/* update spark line chart
 * @param data
 * @param selectedRegion which group in particular (either a specific region, or a year)
 * @param xvar refers to the time variable
 * @param yvar y-variable to be assessed
 */
updateSpark = function(data, selectedRegion, xvar, yvar) {

 //if yvar is different, then update yScale titles, and yvar:
 var timeData = chart.timeData;
  yScale.domain(d3.extent(timeData, function(d) { return d[yvar]; }));

  d3.select(".spark-yaxis")
    .call(d3.axisLeft(yScale));

  d3.select('.spark-title')
    .text(yvar + " by " + xvar);

  d3.select('.y-lab')
    .text(yvar);

  d3.select('.spark-div')
    .classed('hidden', false);

  // line function
  addLines = d3.line()
               .x(function(d) { return (d[xvar] ? xScale(d[xvar]) : xScale(0)); })
               .y(function(d) { return (d[yvar] ? yScale(d[yvar]) : yScale(0)); });

  // remove and replot:
  d3.select('.line-group').selectAll('.spark-line').remove();

  var lines = d3.select('.line-group').selectAll('.spark-line')
                .data(data)
                .enter().append("path")
                .transition()
                .duration(500)
                .attr("class", "spark-line")
                .attr("d", addLines);

  var lineTool = d3.select('.lineTool');
  lineTool.selectAll(".follow-path").remove();

  // add number of circles + text for hover effects:
  var cc = lineTool.selectAll('.follow-path')
                   .data(data)
                   .enter().append("circle")
                   .attr('class', 'follow-path')
                   .attr("r", 3.5);

  lineTool.selectAll('.follow-text').remove();

  var textLabels = lineTool.selectAll('.follow-text')
                           .data(data)
                           .enter().append("text")
                           .attr('class', 'follow-text')
                           .attr("dy", "-0.5em");

  //append region labels
  d3.select('.spark-group').selectAll('.region-lab').remove();

  var regLab = d3.select('.spark-reglab').selectAll('.region-lab')
                 .data(selectedRegion)
                 .enter().append("text")
                 .attr("class", "region-lab")
                 .attr("font-size", "0.75em")
                 .attr("dx", "0.5em")
                 .text(function(d) { return d; })
                 .attr("transform", function(d, i) {
                   return("translate(" + xScale(d3.max(timeData, function(d) { return d[xvar]; }))
                          + "," + yScale(data[i][data[i].length - 1][yvar]) + ")"); });

mousemove = function() {
  // FIXME: this only works if it's a Year variable... switch to time scales?
    var x0 = Math.round((xScale.invert(d3.mouse(this)[0])));

    //extract x, y:
    var circleData = [];
    for (var i = 0; i < data.length; i++) {
      for (var j = 0; j < data[i].length; j++) {
        if (data[i][j][xvar] == x0) {
          circleData.push(data[i][j][yvar]);
        }
      }
    };

    if (circleData) {

      var lineTool = d3.select('.lineTool'),
          width = $('.tbl-div').width();

      // move circles and text according to the mouse...
      cc.data(circleData)
        .attr("transform", function(d) {
                return ("translate(" + xScale(x0) + "," + yScale(+d) + ")");
              });

      textLabels.data(circleData)
                .attr("transform", function(d) {
                        return ("translate(" + xScale(x0) + "," + yScale(+d) + ")")
                      })
                .text(function(d) { return d; });

      } else {
        return ;
    }
}

  d3.select('.spark-overlay')
    .on('mousemove', mousemove);

}

// be able to select different numeric variables:
selectForm = function(ff) {

    // numeric variables in data set:
    var numVar = chart.numVar;
    var yvar = chart.names[chart.names.length - 1];

    // create selection:
    var controlVar = d3.select('#control').append('div')
                       .attr('class', 'form-group ccvar');

    // add label:
    var label = d3.select('.ccvar').append('label')
                  .html('Variable to display:');

    var selection = d3.select('.ccvar').append('select')
                      .attr("class", "form-control control-var")
                      .on('change', ff);

    var options = d3.select('.control-var').selectAll('option')
                    .data(numVar).enter()
                    .append('option')
                    .attr('id', function(d) { return ('option.' + d); } )
                    .attr('value', function(d) { return d; })
                    .html(function(d) { return d; });

  // set default:
  document.getElementById('option.' + yvar).setAttribute('selected', "selected");

  //add title:
  var formTitle = selection.append('p')
                           .attr("class", "form-title")
                           .html("Variable to display");

}

//store circle widths and heights - called before circleControl!
getCircleSize = function() {
  var cc = document.getElementsByClassName('centroid');
  var w = [];
  for (i = 0; i < cc.length; i++) {
    w.push(+cc[i].getAttribute('width'));
  }
  return(d3.extent(w));
}

// circle control - change the sizeby variable (for centroid maps only)
circleControl = function() {

    var newY = d3.select('.control-var').property('value');
    var data = chart.data;
    var csize = circleSize;

    // filter for missing values...
    var arr = [];

    for (i = 0; i < data.length; i++) {
      if (data[i][newY]) {
        arr.push(data[i]);
      } else {
        continue;
      }
    }

  // find the maximum circle
  var max = d3.max(arr, function(d) { return d[newY]; });

  // set a squareroot scale to make circle area proportional to variable:
  var sqrtScale = d3.scaleSqrt()
                  .domain(d3.extent(arr, function(d) { return d[newY]; }))
                  .range(csize);

 // because circles are defined using 'use' elements (with 'shadow DOM' circles)
 // controlling width and height of these 'use' elements control the circle (radius)
 // transform is by half of the width and height
  var circles = d3.selectAll('.centroid')
                  .data(arr)
                  .transition()
                  .duration(300)
                  .attr('width', function(d) {
                      return sqrtScale(d[newY]);
                        })
                  .attr('height', function(d) {
                    return sqrtScale(d[newY]);
                        })
                  .attr("transform", function(d) {
                    var c = - sqrtScale(d[newY])/2;
                        return ("translate(" + c + "," + c + ")");
                  });

  // update tooltips:
  chart.names[2] = newY;

}

// For filtering data and updating line chart when variable changes:
function changeVar() {

  var newY = d3.select('.control-var').property('value');
  var yy = chart.names[2];

  if (yy == newY) {
        // show sparklines on map:
        d3.selectAll('.sparkRegion')
          .classed('hidden', false);
    } else {
        d3.selectAll('.sparkRegion')
          .classed('hidden', true);
        }

        // find selected regions:
        var dd = [];
        var regionsSelected = document.getElementsByClassName('region selected');
          for (var i = 0; i < regionsSelected.length; i++) {
              var id = regionsSelected[i].id;
              var regionName = id.substring(0, id.lastIndexOf('.'));
              dd.push(regionName);
          }

        // for multiples: filter data
            var dt = [];
            group = chart.names[0];
            for (var j = 0; j < dd.length; j++) {
              var updated = filterData(chart.timeData, group, dd[j]);
              dt.push(updated);
            }

        // then update line chart:
        var xvar = chart.names[1];
        updateSpark(dt, dd, xvar, newY);
}

// add a time slider:
addSlider = function() {

    // suggests there are multiple observations over time...
    var seqVar = chart.seqVar[0],
        current = chart.data[0][seqVar],
        range = d3.extent(chart.timeData, function(d) { return d[seqVar]; }),
        int = chart.int[0];

    // input a slider:
    var sliderDiv = d3.select("#control").append("div")
                   .attr("class", "slider-div");

    var sliderTitle = sliderDiv.append("label")
                               .html("By " + seqVar + ":");

    var sliderVal = sliderDiv.append("div")
                             .attr("class", "slider-val")
                             .html(current);

    var slider = sliderDiv.append("input")
                    .attr("class", "slider")
                   .attr("type", "range")
                   .attr("min", range[0])
                   .attr("max", range[1])
                   .attr("step", int)
                   .attr("value", current)
                   .on("input", timeChange);

}

// what happens when the slider moves?
timeChange = function() {

  var cur = this.value;
  d3.select(".slider-val").html(cur);
  // filter data across this time variable:
  var data = filterData(chart.timeData, chart.seqVar[0], cur);
  //sort data relative to how regions are plotted:
  data.sort(function(a, b){
      if(a[chart.names[0]] < b[chart.names[0]]) return - 1;
      if(a[chart.names[0]] > b[chart.names[0]]) return 1;
      return 0;
    });

  var yvar = chart.names[chart.names.length - 1];
  chart.data = data;

  // update regions:
  if (chart.type == "region") {
    updateFills();
  } else {
    circleControl();
  }

    //update tooltips and table:
    makeTooltips(chart);
    var colNum = cnames.findIndex(cnames => cnames == chart.seqVar[0]);
    table.search('').columns().search('').draw();
    table.columns(colNum + 1).search(cur + "|NA", true, false).draw();

}

//screen table if there are multiple observations and is a regional map:
setTable = function() {
  var cur = $(".slider-val").html();
  var colNum = cnames.findIndex(cnames => cnames == chart.seqVar[0]);
  table.columns(colNum + 1).search(cur + "|NA", true, false).draw();
}

addZoom = function(chart) {
  // TODO: make this better...
  // adapted from: https://bl.ocks.org/iamkevinv/0a24e9126cd2fa6b283c6f2d774b69a2
  zoomed = function() {
      bg.setAttribute("transform", d3.event.transform);
  }
    // attempt zooming within the box.
    var zoom = d3.zoom()
                .scaleExtent([1, 8])
                 .on("zoom", zoomed);

    var bg = document.querySelectorAll("g[id^='panel']")[0];
    d3.select(bg)
      .attr('class', 'background')
      .call(zoom);
}

/* set and initialize plot interactivity
 * @param chart all the data exported from R
 * @return a chart with interactivity
*/
setPlot = function(chart) {
    makeTooltips(chart);
    tableToPlot("region");

    if(chart.type == "region") {
      setLegend();
      selectForm(updateFills);
    }

    if (chart.type == "point") {
        circleSize = getCircleSize();
        selectForm(circleControl);
    }

    // add time slider if there are multiple observations:
    if ((chart.type[0] != "sparklines") && (chart.multi[0] == true)) {
        addSlider();
        setTable();
    }

    //if it's got sparklines, add line chart
    if (chart.type == "sparklines") {
        sparkPlot(chart);
        selectForm(changeVar);
    }
    addZoom(chart);
}

// initialize:
setPlot(chart);

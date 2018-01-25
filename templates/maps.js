// ggplot2 maps for iNZightMaps
var svg = d3.select('svg')
            .attr('width', null)
            .attr('height', null);

var table = $('#table').DataTable({
      "colReorder": true,
      "columnDefs": [
        { //hide rowID column
          "targets": [0],
          "visible": false
        }
      ]
  });

showTable = function() {
    var tableWrapper = $('#table_wrapper');
    tableWrapper.toggleClass('hidden');
};

makeTooltips = function(chart) {

    var data = chart.data,
        names = chart.names,
        pathElements = document.querySelectorAll('g[id^="grill.gTree"]~g[id^="GRID.pathgrob"] path'),
        sparkElements = document.querySelectorAll('g[id^="GRID.gTree"] path'),
        regions = d3.selectAll(pathElements)
                    .attr('class', 'region'),
        sparkRegions = d3.selectAll(sparkElements)
                         .attr('class', 'sparkRegion')
        centroids = d3.select('svg').selectAll('use')
                      .attr('class', 'centroid');

    var tooltip = d3.select('body').append('div')
                     .attr('class', 'tooltip')
                     .style('width', '100');

    d3.selectAll('.region')
      .data(data)
      .on('mouseover', function(d, i) {
         var selected = d3.select(this);
        // make things go light and dark:
        selected.classed('selectRegion', true);

        var g = ' ';
        var p = names.length;

        for (var j = 0; j < p; j++) {
            var w = names[j] + ": <span>";
            var t = data[i][names[j]] + "</span> <br />";
            var g = w + t + g;
        }

        // instead just do region:
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

      d3.selectAll('.region')
        .attr("class", function() {
           if (this === selected) {
              return "region selected";
          } else {
            return "region none";
          }
        })

        var j = i;

        // for point maps:
        d3.selectAll('.centroid')
          .attr("class", function(d, i) {
            if (i == j) {
              return("centroid selected");
            } else {
              return("centroid none");
            }
          })

      if (chart.spark[0]) {
          // fade sparkregions:
          sparkRegions.attr('class', function(d, i) {
              if (i == j) {
                  return "sparkRegion selected";
              } else {
                  return "sparkRegion none";
              }
          })

          var group = chart.spark[0],
              xvar =  chart.spark[1],
              yvar = chart.spark[2],
              selectedRegion = data[i][group],
              updated = filterData(chart.timeData, group, selectedRegion);
          // pass the data into the sparklines plot:
          updateSpark(updated, selectedRegion, xvar, yvar);
          table.search('').columns().search('').draw();
          // find column number:
          var colNames = document.getElementsByTagName('th');
          for (var k = 0; k < colNames.length; k++) {
            if(colNames[k].innerHTML == " " + group + " ") {
              var colNum = k;
            }
          }
          table.columns(colNum + 1).search("^" + selectedRegion + "$", true).draw();
      } else {
        //filter table: assuming that each region corresponds to a row
        table.search('').columns().search('').draw();
        table.columns(0).search("^" + (i+1) + "$", true).draw();
      }

    })
};

filterData = function(data, group, region) {
    var ava = [];
    //could possibly use filter?
    for (i = 0; i < data.length; i++) {
        if (data[i][group] == region) {
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
    var ind = table.rows('.active')[0];
    d3.selectAll("." + type)
      .attr("class", function(d, i) {
        return (ind.includes(i) ? type + " selected" : type + " none");
      })
    // for centroids:
    d3.selectAll('.centroid')
      .attr("class", function(d, i) {
        return (ind.includes(i) ? "centroid selected" : "centroid none");
      })
    //for sparklines:
    d3.selectAll('.sparkRegion')
      .attr("class", function(d, i) {
        return (ind.includes(i) ? "sparkRegion selected" : "sparkRegion none");
    });
  })
}

reset = function() {
  //clear map
  d3.selectAll('.region')
    .classed('selected none', false);

  d3.selectAll('.sparkRegion')
    .classed('.sparkRegion none', false);

  d3.selectAll('.centroid')
    .classed('.centroid none', false);
  // clear table
  table.search('').columns().search('').draw();
  table.rows().nodes().to$().removeClass('active');

  d3.select('.spark-div')
    .classed('hidden', true);

  //reset zooming:
  d3.select('.background')
    .attr('transform', null);

}

sparkPlot = function(chart) {
    // get x and y variables
    var xvar = chart.spark[1];
    var yvar = chart.spark[2];

    // TODO: figure out how to make it responsive.
    var sparkContainer = d3.select('.tbl-div').insert('div', ":first-child")
                       .attr('class', 'spark-div')
                       .classed('hidden', true)
                       .style('width', '100%')
                       .style('min-height', '300px')
                       .style('padding', '10px')
                       .style('clear', 'left');

    var margin = {top: 50, right: 50, bottom: 20, left: 50};

    var spark = d3.select('.spark-div').append('svg')
            .attr('class', 'sparkPlot')
            //.attr('width', '100%')
            .attr('viewBox', "0 0 600 400")
            .attr("preserveAspectRatio", "xMinYMin meet");
    var g = spark.append('g').attr('transform', 'translate(' + margin.left + ', ' + margin.top + ')');

    var width = $('.tbl-div').width();
    var height = 300;

    // create x and y scales:
    xScale = d3.scaleLinear()
               .range([0, width]);

    yScale = d3.scaleLinear()
               .range([height, 0]);

    xScale.domain([chart.spark[3], chart.spark[5]]);
    yScale.domain([chart.spark[4], chart.spark[6]]);

    // xaxis:
    g.append("g")
         .attr("class", "spark-xaxis")
         .attr("transform", "translate(0," + (height) + ")")
         .call(d3.axisBottom(xScale)
                 .tickFormat(d3.format("d")));

    //yaxis:
    g.append("g")
         .call(d3.axisLeft(yScale));

    //append paths and bind:
    g.append("path")
         .attr("class", "spark-line")
         //.attr("d", addLines(data))
         .attr("stroke", "black")
         .attr('fill', 'none')
         .attr('stroke-width', 1.5);

    //add axis labels:
    var xlab = spark.append("text")
                    .attr("transform", "translate(" + width/1.75 + "," + (height + margin.top + 35) + ")")
                    .style("text-anchor", "center")
                    .text(xvar);

    var ylab = spark.append("text")
                    .attr("transform", "translate(0," + (margin.top - 10) +")")
                    .style("text-anchor", "center")
                    .text(yvar);

    // add a title for the region:
    var title = spark.append("text")
                     .attr("transform", "translate(" +  (width + margin.right) + "," + (margin.top - 30) + ")")
                     .attr("class", "spark-title")
                     .attr("font-size", "1.5em")
                     .style("text-anchor", "end");


    // add focus lines - adapted from: https://bl.ocks.org/alandunning/cfb7dcd7951826b9eacd54f0647f48d3
    var lineTool = g.append('g')
                    .attr('class', 'lineTool')
                    .style('display', 'none');

    lineTool.append("line")
            .attr("class", "x-line")
            .attr("y1", 0)
            .attr('stroke-dasharray', '0.5, 1')
            .style('stroke', 'black')
            .style('stroke-width', '0.5');

    lineTool.append("line")
            .attr("class", "y-line")
            .attr("x2", 0)
            .attr('stroke-dasharray', '0.5, 1')
            .style('stroke', 'black')
            .style('stroke-width', '0.5');

    lineTool.append("circle")
            .attr("r", 5);

    lineTool.append("text")
            .attr("dx", "-3em")
            .attr("dy", "-1em");

    spark.append("rect")
         .attr("transform", "translate(" + margin.left + "," + margin.top + ")")
         .attr("class", "spark-overlay")
         .attr("width", width)
         .attr("height", height)
         .style('fill', 'transparent')
         .on("mouseover", function() { lineTool.style("display", null); })
         .on("mouseout", function() { lineTool.style("display", "none"); });

}

updateSpark = function(data, selectedRegion, xvar, yvar) {
  // with the filtered data... select all paths:
  d3.select('.spark-title')
    .text(selectedRegion);

  d3.select('.spark-div')
    .classed('hidden', false);

  var lines = d3.select('.spark-line')
                .data(data);

  // line function
   addLines = d3.line()
               .x(function(d) { return xScale(d[xvar]); })
               .y(function(d) { return yScale(d[yvar]); });

  //update:
  lines.transition()
       .duration(500)
       .attr("d",addLines(data));

mousemove = function() {
  // TODO: this only works if it's a Year variable...
    var x0 = Math.round((xScale.invert(d3.mouse(this)[0])));

    for (var i = 0; i < data.length; i++) {
      if (data[i][xvar] == x0) {
        var d = data[i];
      }
    }

    if (d) {
      var lineTool = d3.select('.lineTool'),
          width = $('.tbl-div').width(),
          height = 300;
    //set focus tooltip:
    lineTool.attr("transform", "translate(" + xScale(d[xvar]) + "," + yScale(d[yvar]) + ")");
    lineTool.select("text").text(function() { return yvar + ": " + d[yvar]; });
    lineTool.select(".x-line").attr("y2", height - yScale(d[yvar]));
    lineTool.select(".y-line").attr("x1", - xScale(d[xvar]));
  } else {
    return ;
  }
}

  d3.select('.spark-overlay')
    .on('mousemove', mousemove);

}

addZoom = function(chart) {
  // TODO: make this better...
  // adapted from: https://bl.ocks.org/iamkevinv/0a24e9126cd2fa6b283c6f2d774b69a2
  zoomed = function() {
      // not exactly the best kind of thing?
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

setPlot = function(chart) {
    makeTooltips(chart);
    tableToPlot("region");
    //if it's got sparklines, add line chart
    if (chart.spark[0]) {
        sparkPlot(chart);
    }
    addZoom(chart);
}

// initialize:
setPlot(chart);

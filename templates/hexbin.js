// For hexagonal bin plots:

//restrict svg-container for drag box to align
var container = document.getElementById('svg-container');
container.classList.add('contained');

//hide viewtable button
var viewTable = document.getElementById("viewTable");
viewTable.classList.add('hidden');

var count = counts.length;

var svg = document.getElementsByTagName('svg')[0];

// Extending rectangle:
var rect = document.getElementsByTagName('rect')[0];
extendPlotRegion(rect);

// Identifying hexbins:
 var Grob = getGrob('hexbin');

// creating labels:
  for (i = 1; i <= count; i++) {
    gLabel(i);
  }

  for (i = 1; i <= count; i++) {
    gRect(i);
  }

  for (i  = 1; i <= count; i++) {
  var hexbin = document.getElementById(Grob + '.' + i);
    hexbin.setAttribute("class", 'hexbin');

    var coords = hexbin.getAttribute('points'),
        coordsxy = coords.split(" ")[5],
        x = Number(coordsxy.split(",")[0]), //co-ordinates based upon SVG elements.
        y = Number(coordsxy.split(",")[1]);
    label('label', 'N = ', i, 45); // deal with t-span later.
    tLabel('tLabel', counts[i-1] + ', ' + (counts[i-1]/n*100).toFixed(2) + "%", i, document.getElementById('label' + i));

// labels for where hexbin is centered at:
    label('hexCoord', 'Centered at: ', i, 30);
    label('hx_hy', xcm[i-1] + ', ' + ycm[i-1], i, 15);

      // Attach and draw rectangles to labels according to the size of the gLabel (with all labels attached)
          drawRectLabel(i);
  };


  //interactivity: - similar to one way hexbin plots
  for (i = 1; i <= count; i++) {
   (function(i){
     var hexbin = document.getElementById(Grob + '.' + i);
     hexbin.addEventListener("mouseover",function(){light(i, 'light')},false);
     hexbin.addEventListener("mouseout", function(){normal(i, 'light')}, false);
     hexbin.addEventListener("click", function(){fade(i)}, false); // defined below.
     }) (i)
   }


   fade = function(i) {
     for (j = 1; j <= count; j ++) {

       var hexbin = document.getElementById(Grob + '.' + j);
       var gLabel = document.getElementById('gLabel' + j);

       if (i == j) {
         hexbin.setAttribute('class', 'hexbin selected');
         gLabel.classList.remove('invisible');
       } else {
         hexbin.setAttribute('class', 'hexbin none');
         gLabel.classList.add('invisible');
       }
   }
   };

   /* --------------------------------------------
     User selection drag box:
   ---------------------------------------------*/

     //create invisible selection box that is enabled when dragging occurs:
  createSelectionBox(Grob);

   var evt = window.event;
   var zoomBox = {};

   svg.setAttribute('onmouseup', 'MouseUp(evt)');
   svg.setAttribute('onmousemove', 'MouseDrag(evt)');
   svg.setAttribute('onmousedown', 'MouseDown(evt)'); //defined below.

   MouseDrag = function(evt) {
       if(zoomBox["isDrawing"]) {
         svg.style.cursor = "crosshair";
           zoomBox["endX"] = evt.pageX - 20;
           zoomBox["endY"] = evt.pageY - 20;

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

           selectRect.setAttribute('points', x1 + ',' + y1 + " " + x1 + ',' + y2 + ' '
                                             + x2 + ',' + y2 + ' ' + x2 + ',' + y1);

          // information to extract:
          var groupN = [];

           for (i =1; i <= count; i++) {
           var hexbin = document.getElementById(Grob + '.' + i);
           var gLabel = document.getElementById('gLabel' + i);

           //find the x value at the center of the hexagon (midpoint):
           var coords = hexbin.getAttribute('points');
           var topEdge = coords.split(' ')[5];
           var bottomEdge = coords.split(' ')[2];
           var x = Number(topEdge.split(",")[0]); //midpoint x
           var y = (Number(topEdge.split(",")[1]) + Number(bottomEdge.split(",")[1]))/2; //midpoint y

             if (hexbin.getAttribute('visibility') == 'hidden') {
               // those that are hidden, remain hidden
                 hexbin.classList.add('hidden');

               } else {
                 //points that lie within the  boundary box drawn:
             if((x1 <= x && x <= x2) && (y1 <= y && y <= y2)) {
               hexbin.setAttribute('class', ' hexbin selected');
               groupN.push(counts[i-1]); // store no. of couns from each hexbin that's selected

              } else {
                hexbin.setAttribute('class', 'hexbin none');
              }
            }
           }

           //summation of array:
           var sum = groupN.reduce(function(a, b) { return a + b; }, 0);
           //console.log(sum);

           // report a proportion:
           var nProp = (sum/n*100).toFixed(2) + "%";

           var selectedGroup = document.getElementById('selectionLabelGroup');
           if (selectedGroup !== null) {
               selectedGroup.remove();
              }
              //create new label group:
              var selectionLabelGroup = createSelectionLabelGroup();

             //information to extract:
           var nbins = document.getElementsByClassName('selected').length;

          //create labels:
          selectionLabel('groupN', (x1+x2)/2, y1-15, 'Frequency: ');
          tLabel('groupNval', sum + ', ' + nProp, 0, document.getElementById('groupN'));

          selectionLabel('nbins', (x1+x2)/2, y1-30, 'bins: ');
          tLabel('nbinVal', nbins, 0, document.getElementById('nbins'));

          // Attach and draw rectangle to label:
            drawSelectRectLabel(selectionLabelGroup);

       }
   };


   // Reset button:
   reset = function() {
     for (i =1; i <= count; i++) {
      var hexbin = document.getElementById(Grob + '.' + i);
      var gLabel = document.getElementById('gLabel' + i);

      gLabel.classList.add('invisible');
      hexbin.setAttribute('class', 'hexbin');
   }

   var selectRect = document.getElementById('selectRect');
   if (selectRect.getAttribute('points') !== null) {
     selectRect.removeAttribute('points');
 }
  var selectionLabelGroup = document.getElementById('selectionLabelGroup');
     if (selectionLabelGroup !== null || undefined) {
       selectionLabelGroup.remove();
     }
 };

// For hexagonal bin plots:

//restrict svg-container for drag box to align
var container = document.getElementById('svg-container');
container.classList.add('contained');

var viewTable = document.getElementById("viewTable");
viewTable.classList.add('hidden');

//parsing data through...
var xcm  = JSON.parse(xcm);
var ycm  = JSON.parse(ycm);
var counts = JSON.parse(counts);

var count = counts.length;

var svg = document.getElementsByTagName('svg')[0];

// Extending rectangle:
var rect = document.getElementsByTagName('rect')[0];
rect.setAttribute('width', rect.getAttribute('width')*2);
rect.setAttribute('height', rect.getAttribute('height')*2);
rect.setAttribute('x', rect.getAttribute('x')-20);
rect.setAttribute('y', rect.getAttribute('y')-20);

// Identifying hexagons:
p = document.getElementsByTagName('polygon')[0];
var id = p.getAttribute('id');
var Grob = id.substring(0, id.lastIndexOf('.'));

//creating group labels:
gLabel = function(i) {
var panel = document.getElementsByTagName('g')[0];
var gEl = document.createElementNS("http://www.w3.org/2000/svg", "g");
    gEl.setAttributeNS(null, 'id', 'gLabel' + i);
    gEl.setAttributeNS(null, 'class', 'gLabel invisible');
    panel.appendChild(gEl);
  }

//function to create rectangles for labels:
gRect = function(i) {
    var gRect = document.createElementNS("http://www.w3.org/2000/svg", "rect");
        gRect.setAttributeNS(null, 'id', 'gRect' + i);
        gRect.setAttributeNS(null, 'class', 'gRect');
    gLabel = document.getElementById('gLabel' + i);
    gLabel.appendChild(gRect);
  };

  for (i = 1; i <= count; i++) {
    gLabel(i);
  }

  for (i = 1; i <= count; i++) {
    gRect(i);
  }


  label = function(id, textinput, i, tf) {
  //attributes for the text SVG element
    var label = document.createElementNS("http://www.w3.org/2000/svg", "text");
      label.setAttributeNS(null, 'transform', 'translate('+ (x) + ', ' + (y + tf) +') scale(1, -1)');
      label.setAttributeNS(null, 'id', id + i);
      label.setAttributeNS(null, 'class', 'label' + ' ' + id);

  // Creating the text label element:
    var textNode = document.createTextNode(textinput);

      label.appendChild(textNode);
      var gLabel = document.getElementById('gLabel' + i);
      gLabel.appendChild(label);
  };

  //creating tspan labels - for customizing text in bold:
  tLabel = function(id, textinput, i, lab) {
    var tLabel = document.createElementNS("http://www.w3.org/2000/svg", "tspan");
    tLabel.setAttributeNS(null, 'class', 'tLabel' + ' ' + id);
    tLabel.setAttributeNS(null, 'id', id + i);

    var textNode = document.createTextNode(textinput);
    tLabel.appendChild(textNode);
    lab.appendChild(tLabel);

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
          var gLabel = document.getElementById('gLabel' + i),
              rectParam = gLabel.getBBox(),
              gRect = document.getElementById('gRect' + i);
          gRect.setAttribute('x', rectParam.x-5);
          gRect.setAttribute('y', rectParam.y-2);
          gRect.setAttribute('width', rectParam.width+10);
          gRect.setAttribute('height', rectParam.height+4);

  };


  //interactivity: - similar to one way hexbin plots
  for (i = 1; i <= count; i++) {
   var hexbin = document.getElementById(Grob + '.' + i);
   hexbin.setAttribute('onmouseover', 'light('+ i + ')');
   hexbin.setAttribute('onmouseout', 'normal(' + i +')');
   hexbin.setAttribute('onclick', 'fade(' + i +')');
   }

   light = function(i) {
     var hexbin = document.getElementById(Grob + '.' + i);
     hexbin.classList.add('light');

     var gLabel = document.getElementById('gLabel' + i);
     gLabel.classList.remove('invisible');
   };

   normal = function(i) {
     var hexbin = document.getElementById(Grob + '.' + i);
     hexbin.classList.remove('light');

     var gLabel = document.getElementById('gLabel' + i);
     gLabel.classList.add('invisible');
   };

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

   // Adding selection box:
   // Trialling a select drag box:
   //obtaining svg region:

   var rect = document.getElementsByTagName('rect');
   var width = svg.width.baseVal.value;
   var height = svg.height.baseVal.value;

   //putting selection rectangle in a group element:
   var g = document.createElementNS('http://www.w3.org/2000/svg', 'g');
     g.setAttributeNS(null, 'id', 'selectionBox');
     var panel = document.getElementById(Grob);
     panel.appendChild(g);


   var evt = window.event;

   svg.setAttribute('onmouseup', 'MouseUp(evt)');
   svg.setAttribute('onmousemove', 'MouseDrag(evt)');
   svg.setAttribute('onmousedown', 'MouseDown(evt)');

//create selectionBox:
     var selectRect = document.createElementNS('http://www.w3.org/2000/svg', 'polygon');
     selectRect.setAttributeNS(null, 'id', 'selectRect');
     selectRect.setAttributeNS(null, 'class', 'selectRect');
     g.appendChild(selectRect);

  //create a new label:
  selectionLabel = function(id, x, y, textinput) {
  var selectionLabel = document.createElementNS("http://www.w3.org/2000/svg", 'text');
       selectionLabel.setAttributeNS(null, 'class', 'label');
       selectionLabel.setAttributeNS(null, 'id', id);
       selectionLabel.setAttributeNS(null, 'transform', 'translate(' + x +  ',' + y + ') scale(1, -1)');
      var text = document.createTextNode(textinput);
      selectionLabel.appendChild(text);

      var selectionLabelGroup = document.getElementById('selectionLabelGroup');
       selectionLabelGroup.appendChild(selectionLabel);
     }


   var zoomBox = {};

   MouseDown = function(evt) {

     var selectedGroup = document.getElementById('selectionLabelGroup');
     if (selectedGroup !== null) {
         selectedGroup.remove();
        }

       zoomBox["startX"] = evt.pageX - 20; // 20 comes from the padding added to the body.
       zoomBox["startY"] = evt.pageY - 20;
       zoomBox["isDrawing"] = true;
      selectRect.setAttribute('points',  zoomBox["startX"] + ',' + zoomBox["startY"]);
   };

   MouseUp = function(evt) {
     svg.style.cursor = "default";
         zoomBox["endX"] = evt.pageX - 20;
         zoomBox["endY"] = evt.pageY - 20;
         zoomBox["isDrawing"] = false;
     };

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

          //create group label by selection:
          //grouping it together!:
          var selectionLabelGroup = document.createElementNS("http://www.w3.org/2000/svg", 'g');
              selectionLabelGroup.setAttributeNS(null, 'class', 'gLabel');
              selectionLabelGroup.setAttributeNS(null, 'id', 'selectionLabelGroup');
          var panel = document.getElementsByTagName('g')[0];
              panel.appendChild(selectionLabelGroup);
        //make a rectangle for this special label:
          var selectionLabelRect = document.createElementNS("http://www.w3.org/2000/svg", 'rect');
              selectionLabelRect.setAttributeNS(null, 'class', 'gRect');
              selectionLabelRect.setAttributeNS(null, 'id', 'selectionLabelRect');
              selectionLabelGroup.appendChild(selectionLabelRect);

             //information to extract:
           var nbins = document.getElementsByClassName('selected').length;

          //create labels:
          selectionLabel('groupN', (x1+x2)/2, y1-15, 'Frequency: ');
          tLabel('groupNval', sum + ', ' + nProp, 0, document.getElementById('groupN'));

          selectionLabel('nbins', (x1+x2)/2, y1-30, 'bins: ');
          tLabel('nbinVal', nbins, 0, document.getElementById('nbins'));

          // Attach and draw rectangle to label:
            var sRectParam = selectionLabelGroup.getBBox();
            selectionLabelRect.setAttribute('x', sRectParam.x-5);
            selectionLabelRect.setAttribute('y', sRectParam.y-2);
            selectionLabelRect.setAttribute('width', sRectParam.width+10);
            selectionLabelRect.setAttribute('height', sRectParam.height+4);

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
   var selectionLabelGroup = document.getElementById('selectionLabelGroup');
   if (selectRect.getAttribute('points') !== null) {
     selectRect.removeAttribute('points');
     selectionLabelGroup.remove();
   }
 };

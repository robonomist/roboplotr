function getVerticalLayout(el, legend_fontsize, height = false, keys, pie_chart, showfinal = false) {
  if(height == false) {height = el.layout.height};
	let elcontainer = height;
	let eltitle = $(el).find('g.g-gtitle')[0].getBBox();
	let elslider = 0;
	if ('rangeslider' in el.layout.xaxis) {
	  if(el.layout.xaxis.rangeslider.visible == true) {
	    elslider = $(el).find('g.infolayer > g.rangeslider-container')[0].getBBox().height
	  }
	  }
	let elxticks = $(el).find('g.xaxislayer-above')
	if(elxticks.length > 0) { elxticks = elxticks[0].getBBox().height } else { elxticks =  0 };
	let margin_top = eltitle.height+el.layout.title.font.size*2;
	let elcaption = $(el).find('g.annotation')[0].getBBox().height;
	let ellegend = elxticks;
	if ($(el).find('g.legend')[0] != undefined) { ellegend =  $(el).find('g.legend')[0].getBBox().height };
	let margin_bottom = ellegend + elcaption + elxticks;
	let margin_bottom_disp = elslider * 1//elslider > 0 ? margin_bottom * 0.1 : 0;
	let elplot = pie_chart ? $(el).find('.pielayer > .trace > .slice > .surface') : $(el).find('.cartesianlayer > .xy > .gridlayer > .x');
	if (elplot.length > 0) {elplot = elplot[0].getBBox().height};
	let images_sizey = elcontainer / 20 / elplot;
	let elimages = $(el).find('g.layer-above > g.imagelayer')[0].getBBox().height;
	let legend_y = -((margin_bottom - elcaption + (elslider+margin_bottom_disp)) / elplot);
	if (legend_y < -3) {
	  Plotly.relayout(el, {'xaxis.rangeslider.visible': false})
	  elslider = 0;
	  let elxticks = $(el).find('g.xaxislayer-above')
	  if(elxticks.length > 0) { elxticks = elxticks[0].getBBox().height } else { elxticks = 0 };
  	ellegend = $(el).find('g.legend')[0].getBBox().height;
	  margin_bottom = ellegend + elcaption + elxticks;
	  margin_bottom_disp = 0//elslider > 0 ? margin_bottom * 0.1 : 0;
	  let elplot = pie_chart ? $(el).find('.pielayer > .trace > .slice > .surface') : $(el).find('.cartesianlayer > .xy > .gridlayer > .x');
	  if (elplot.length > 0) {elplot = elplot[0].getBBox().height};
	  images_sizey = elcontainer / 20 / elplot;
	  elimages = $(el).find('g.layer-above > g.imagelayer')[0].getBBox().height;
	  legend_y = -((margin_bottom - elcaption + (elslider+margin_bottom_disp)) / elplot);
	}
	let title_breaks = (el.layout.title.text.match(new RegExp("<br>", "g")) || []).length;
	let title_y = 1 + ((margin_top+el.layout.title.font.size*title_breaks*2) / elplot)
//	console.log(title_y)
	let annotations_y = -((margin_bottom + (elslider+margin_bottom_disp) - legend_fontsize / 2) / elplot);
	let images_y = -((margin_bottom + (elslider+margin_bottom_disp) - legend_fontsize / 2 - (elimages / 10)) / elplot);
	let legend_font_size = (ellegend > (elplot / 2)) ? legend_fontsize - 2 : legend_fontsize;

  let yaxis_font_size = legend_fontsize

  let yaxis_layer = $(el).find('g.yaxislayer-above')

  if(yaxis_layer.length > 0)
	if( yaxis_layer[0].getBBox().height <= ( ($(el).find('g.ytick')).length * $(el).find('g.ytick')[0].getBBox().height )) {
	  yaxis_font_size = el.layout.yaxis.tickfont.size - 2
	} else {
	  yaxis_font_size = legend_fontsize
	}

	if(showfinal == true) {console.log('legend ht: ' + ellegend +
	' slider ht: ' + elslider +
	' plot area ht: ' + elplot +
	' bottom margin: ' + margin_bottom +
	' container ht: ' + elcontainer,
	' legend position: ' + legend_y,
	' caption position: ' + annotations_y,
	)}
	let thearray = {
	  'title.y': title_y,
		'legend.y': legend_y,
		'legend.yanchor': "bottom",
		'images[0].y': images_y,
		'images[0].sizey': images_sizey,
		'margin.t': margin_top,
		'margin.b': margin_bottom,
		'annotations[0].y': annotations_y,
		'annotations[1].y': annotations_y,
		'annotations[2].y': annotations_y,
		'annotations[3].y': annotations_y,
		'legend.font.size': legend_font_size,
		'yaxis.tickfont.size': yaxis_font_size
	};
//	const inclusivePick = (obj, keys) => Object.fromEntries(
//  keys.map(key => [key, obj[key]]));

  let rearray = keys.reduce(function (obj2, key) {
  if (key in thearray) // line can be removed to make it inclusive
    obj2[key] = thearray[key];
    return obj2;

  }, {});


//  let rearray = inclusivePick(thearray, keys)
	return rearray;

}

function setVerticalLayout(eventdata, gd, legend_fontsize, alt_title, pie_chart) {
	if ('width' in eventdata) {
	  if ('rangeslider' in gd.layout.xaxis) {
	  if (gd.layout.xaxis.rangeslider.visible == false) {
	    gd.layout.xaxis.rangeslider.visible = true;
	    Plotly.react(gd,gd.data, gd.layout)
	    //Plotly.relayout(gd, {'xaxis.rangeslider.visible': true})
	  }}
	  let gdtitle = $(gd).find('g.g-gtitle')[0].getBBox();
	  let titlespace = $(gd).find('svg.main-svg')[0].getAttribute("width") * 0.85
	   if (titlespace < gdtitle.width) {
	      //console.log("title too tight!")
	      Plotly.relayout(gd, {'title.text': alt_title[1]})
	   } else if (titlespace > gdtitle.width * 1.8) {
	     Plotly.relayout(gd, {'title.text': alt_title[0]})
	   };
		Plotly.relayout(gd, getVerticalLayout(gd, legend_fontsize, false, keys = ['legend.font.size','images[0].sizey'], pie_chart = pie_chart));
		Plotly.relayout(gd, getVerticalLayout(gd, legend_fontsize, false, keys = ['margin.t','margin.b','legend.y','images[0].y'], pie_chart = pie_chart));
		Plotly.relayout(gd, getVerticalLayout(gd, legend_fontsize, false, keys = ['margin.t', 'margin.b','images[0].sizey', 'yaxis.tickfont.size'], pie_chart = pie_chart));
		Plotly.relayout(gd,
		  getVerticalLayout(gd, legend_fontsize, false,
		  keys = ['images[0].y', 'annotations[0].y', 'annotations[1].y', 'annotations[2].y', 'annotations[3].y', 'legend.y', 'legend.yanchor', 'title.y', 'yaxis.tickfont.size'],
		  pie_chart = pie_chart));
//		console.log("FINAL CHECK")
//		getVerticalLayout(gd, legend_fontsize, false, [""], pie_chart = pie_chart, showfinal = true);
//    console.log(gd.layout.shapes)
	}
}

function yrangeRelayout(eventdata, gd, timerId, trace_sums) {
	if (Object.prototype.toString.call(eventdata['xaxis.range']) === '[object Array]') {
		//console.log(\"rangeslider event!!\");
		var xRange = gd.layout.xaxis.range;
		var yRange = gd.layout.yaxis.range;
		var yInside = [];
		let yStackInside = [];
//		var xInside = [];
		var visdata = gd.data.filter(trace => trace.visible === true || !(trace.hasOwnProperty('visible')));

		visdata.forEach(trace => {
			var len = trace.y.length;//Math.min(trace.x.length, trace.y.length);
//			console.log(trace.type)
			for (var i = 0; i < len; i++) {
				var x = trace.x[i];
				var y = trace.y[i];

				if (x >= xRange[0] && x <= xRange[1]) {
//					xInside.push(x);
					yInside.push(y);
					if(trace.type == "bar") {
					 let obj = {date: x, val: y}
					yStackInside.push(obj)
					}
				}
			}
		});

//console.log(trace_sums)
if(trace_sums == true) {
 var holdermax = {};
yStackInside.forEach(function(d) {
  if (holdermax.hasOwnProperty(d.date) && d.val > 0) {
    holdermax[d.date] = holdermax[d.date] + d.val;
  } else if (d.val > 0) {
    holdermax[d.date] = d.val;
  }
});

var holdermin = {};
yStackInside.forEach(function(d) {
  if (holdermin.hasOwnProperty(d.date) && d.val <= 0) {
    holdermin[d.date] = holdermin[d.date] + d.val;
  }  else if (d.val <= 0) {
    holdermin[d.date] = d.val;
  }
});

yStackInside = [{val: 0}];
for (var prop in holdermin) {
  yStackInside.push({ date: prop, val: holdermin[prop] });
}
for (var prop in holdermax) {
  yStackInside.push({ date: prop, val: holdermax[prop] });
}

yInside = yInside.concat(yStackInside.map(o => o.val));
}

		let yMax = Math.max(...yInside);
		yMax = yMax < 0 ? yMax * 0.95 : yMax * 1.05;
		let yMin = Math.min(...yInside);
		yMin = yMin < 0 ? yMin * 1.05 : yMin * 0.95;
		if('shapes' in gd.layout) {
		  var update = {
			'yaxis.range': [yMin,yMax], 'shapes[0].x0': xRange[0]  // updates the end of the yaxis range
		}
		} else {
		 		var update = {
			'yaxis.range': [yMin,yMax]//, 'shapes[0].x0': xRange[0]  // updates the end of the yaxis range
		};
		}
		Plotly.relayout(gd, update);
		if (timerId >= 0) {
			//timer is running: stop it
			window.clearTimeout(timerId);
		}
		timerId = window.setTimeout(function() {
			//fire end event
			//console.log('rangeslider event ENDS');
			//reset timer to undefined
			timerId = -1;
		}, 800);
	}
}

function plotlyRelayoutEventFunction(eventdata, gd, legend_fontsize, alt_title, rangeslider_sums, pie_chart) {
	timerId = 0;
	setVerticalLayout(eventdata, gd, legend_fontsize, alt_title, pie_chart);
	yrangeRelayout(eventdata, gd, timerId, rangeslider_sums);
};

function getVerticalLayout(el, legend_fontsize, height = false, keys, pie_chart, logo = undefined, showfinal = false) {

  let elcontainer = $(el).find("svg.main-svg")[0].height.animVal.value;
  let eltitle = $(el).find('g.g-gtitle')[0].getBBox();
  let elslider = 0;
  if ('rangeslider' in el.layout.xaxis) {
    if(el.layout.xaxis.rangeslider.visible == true) {
      elslider = $(el).find('g.infolayer > g.rangeslider-container')[0].getBBox().height
    }
  }
  let elxticks_default = pie_chart ? 5 : 0
  let elxticks = $(el).find('g.xaxislayer-above')
  if(elxticks.length > 0) { elxticks = elxticks[0].getBBox().height } else { elxticks =  elxticks_default };
  let modebar_ht = 0;
  let elmodebar = $(el).find('div.modebar');
  if(elmodebar.length > 0) {
    modebar_ht = elmodebar[0].clientHeight;
  }
  let margin_top = eltitle.height + modebar_ht + 11;
  let elcaption = $(el).find('g.annotation')[0].getBBox().height + 5;
  let elxtitle = $(el).find('g.g-xtitle')
  if(elxtitle.length > 0) {
    elxtitle = elxtitle[0].getBBox().height * 1.3;
  } else {
    elxtitle = 0
  }
  let ellegend = {width: 0, height: 0}
  if ($(el).find('g.legend')[0] != undefined) {
    ellegend.height =  Math.ceil($(el).find('g.legend')[0].getBBox().height)
    ellegend.width =  $(el).find('g.legend')[0].getBBox().width
  };
  let margin_bottom = ellegend.height + 10 + (elcaption + elxticks + elxtitle + elslider*1);
  // pie charts do not give correct height by measuring the plot, investigate
  let elplot = pie_chart ? $(el).find('.pielayer') : $(el).find('.gridlayer');
  if (elplot.length > 0) {elplot = elplot[0].getBBox()};
  if (pie_chart) { elplot.height = elcontainer-margin_bottom-margin_top }
  if (elplot.height == 0) { elplot.height = 1}
  let elimages = $(el).find('g.layer-above > g.imagelayer > image')[0].getBBox();
  margin_bottom = margin_bottom + logoSpace(logo, elimages, margin_bottom, elxtitle, elxticks, ellegend);
  let images_sizey = (elcontainer * 0.05) / elplot.height;
  el.layout.images[0].sizey = images_sizey
  let legend_y = -((elxticks + 5 + (elslider*2) + elxtitle) / elplot.height)//((margin_bottom - elcaption + (elslider*2)) / elplot);

  if (legend_y < -2 || (elplot.height < (elcontainer / 4))) {
//    console.log("TOO TOIT")
    if ('rangeslider' in el.layout.xaxis) {
      if(el.layout.xaxis.rangeslider.visible == true) {
        Plotly.relayout(el, {"showlegend" : false, 'xaxis.rangeslider.visible': false })
      } else { Plotly.relayout(el, {"showlegend" : false}) }
    } else {
      Plotly.relayout(el, {"showlegend" : false})
    }
    elslider = 0;
    elxticks = $(el).find('g.xaxislayer-above')
    if(elxticks.length > 0) { elxticks = elxticks[0].getBBox().height } else { elxticks = elxticks_default };
/*    if ($(el).find('g.legend')[0] != undefined) {
      ellegend.height =  Math.ceil($(el).find('g.legend')[0].getBBox().height + 5)
      ellegend.width =  $(el).find('g.legend')[0].getBBox().width
    };*/
    margin_bottom = 10 + elcaption + elxticks + elxtitle;
    margin_bottom = margin_bottom + logoSpace(logo, elimages, margin_bottom, elxtitle, elxticks, ellegend);
    let elplot = pie_chart ? $(el).find('.pielayer') : $(el).find('.gridlayer');
    if (elplot.length > 0) {elplot = elplot[0].getBBox().height};
    if (elplot.height == 0) { elplot.height = 1}
    images_sizey = (elcontainer * 0.05) / elplot.height;
    legend_y = -((margin_bottom - elcaption) / elplot.height);
  }  else if ((elplot.height > (elcontainer / 1.5)) & el.layout.showlegend == false) {
//    console.log("NUFF SPACE")
    if ('rangeslider' in el.layout.xaxis) {
      if(el.layout.xaxis.rangeslider.visible == false) {
        Plotly.relayout(el, {"showlegend" : true, "xaxis.rangeslider.visible": true});
       } else {
          Plotly.relayout(el, {"showlegend" : true});
    }
  } else {
        Plotly.relayout(el, {"showlegend" : true});
  }
    let elslider = 0;
  if ('rangeslider' in el.layout.xaxis) {
    if(el.layout.xaxis.rangeslider.visible == true) {
      elslider = $(el).find('g.infolayer > g.rangeslider-container')[0].getBBox().height
    }
  }
  let elxticks_default = pie_chart ? 5 : 0
  let elxticks = $(el).find('g.xaxislayer-above')
  if(elxticks.length > 0) { elxticks = elxticks[0].getBBox().height } else { elxticks =  elxticks_default };
  let modebar_ht = 0;
  let elmodebar = $(el).find('div.modebar');
  if(elmodebar.length > 0) {
    modebar_ht = elmodebar[0].clientHeight;
  }
  let margin_top = eltitle.height + modebar_ht + 11;
  let elcaption = $(el).find('g.annotation')[0].getBBox().height + 5;
  let elxtitle = $(el).find('g.g-xtitle')
  if(elxtitle.length > 0) {
    elxtitle = elxtitle[0].getBBox().height * 1.3;
  } else {
    elxtitle = 0
  }
  let ellegend = {width: 0, height: 0}
  if ($(el).find('g.legend')[0] != undefined) {
    ellegend.height =  Math.ceil($(el).find('g.legend')[0].getBBox().height)
    ellegend.width =  $(el).find('g.legend')[0].getBBox().width
  };
  margin_bottom = ellegend.height + 10 + (elcaption + elxticks + elxtitle + elslider*1);
  margin_bottom = margin_bottom + logoSpace(logo, elimages, margin_bottom, elxtitle, elxticks, ellegend);
  // pie charts do not give correct height by measuring the plot, investigate
  elplot = pie_chart ? $(el).find('.pielayer') : $(el).find('.gridlayer');
  if (elplot.length > 0) {elplot = elplot[0].getBBox()};
  if (pie_chart) { elplot.height = elcontainer-margin_bottom-margin_top }
  if (elplot.height == 0) { elplot.height = 1}
  elimages = $(el).find('g.layer-above > g.imagelayer > image')[0].getBBox();
  images_sizey = (elcontainer * 0.05) / elplot.height;
  el.layout.images[0].sizey = images_sizey
  legend_y = -((elxticks + 5 + (elslider*2) + elxtitle) / elplot.height)//((margin_bottom - elcaption + (elslider*2)) / elplot);
  }
  let title_y = (elcontainer - (21+modebar_ht)) / elcontainer
  let low_bound_adjust = elxticks == 0 ? -elcaption : 0
//  low_bound_adjust = ellegend.height == 0 ? low_bound_adjust+(elcaption) : low_bound_adjust
  elplot = pie_chart ? $(el).find('.pielayer') : $(el).find('.gridlayer');
  if (elplot.length > 0) { elplot = elplot[0].getBBox() };
  if (pie_chart) { elplot.height = elcontainer-margin_bottom-margin_top }
  if (elplot.height == 0) { elplot.height = 1}
  let low_bound = -((margin_bottom+elslider+low_bound_adjust) / elplot.height)//-Math.min((elcontainer - (elplot.height+margin_top)) / elplot.height,(margin_bottom / elplot.height))
  let annotations_y = low_bound
  let images_y = low_bound
  let legend_font_size = (ellegend.height > (elplot.height / 2)) ? legend_fontsize - 2 : legend_fontsize;
  legend_font_size = (ellegend.width > $(el).find("svg.main-svg")[0].width.animVal.value) ? legend_font_size - 2 : legend_font_size;

  let yaxis_font_size = legend_fontsize

  let yaxis_layer = $(el).find('g.yaxislayer-above')

  if(yaxis_layer.length > 0) {
    let yticks = $(el).find('g.ytick');
    let yspace = [...yticks].reduce((a,b) => a + b.getBBox().height, 0)
    if( elplot.height <= yspace) {
      yaxis_font_size = Math.floor(el.layout.yaxis.tickfont.size * 0.8)
    } else {
      yaxis_font_size = Math.min(Math.floor(el.layout.yaxis.tickfont.size*1.5), legend_fontsize)
    }
  }
  if(showfinal == true) {console.log('legend ht: ' + ellegend.height +
                                       ' slider ht: ' + elslider +
                                       ' plot area ht: ' + elplot.height +
                                       ' plot caption ht: ' + elcaption +
                                       ' bottom margin: ' + margin_bottom +
                                       ' container ht: ' + elcontainer,
                                     ' legend position: ' + legend_y,
                                     ' caption position: ' + annotations_y,
  )}
  let thearray = {
    'title.y': title_y,
    'legend.y': legend_y,
    'images[0].y': images_y,
    'images[0].sizey': images_sizey,
    'margin.t': margin_top,
    'margin.b': margin_bottom,
    'annotations[0].y': annotations_y,
    'legend.font.size': legend_font_size,
    'yaxis.tickfont.size': yaxis_font_size
  };

  let rearray = keys.reduce(function (obj2, key) {
    if (key in thearray) // line can be removed to make it inclusive
    obj2[key] = thearray[key];
    return obj2;

  }, {});

  return rearray;

}

function calculateDisplayedImageSize(imageAspectRatio, container) {
  let containerAspectRatio = container.width / container.height;
  let displayedWidth, displayedHeight;
  if (containerAspectRatio > imageAspectRatio) {

    displayedHeight = container.height;
    displayedWidth = displayedHeight * imageAspectRatio;
  } else {

    displayedWidth = container.width;
    displayedHeight = displayedWidth / imageAspectRatio;
  }

  let result = { width: Math.round(displayedWidth), height: Math.round(displayedHeight) }

  return result;
}

function logoSpace(ratio, image, margin_b, title_h, xtick_h, legend) {
    let logo_actual = calculateDisplayedImageSize(ratio, image)
    let logo_space = [];
    logo_space.horizontal = (image.width - legend.width) - logo_actual.width;
    logo_space.vertical = margin_b - (title_h + xtick_h + legend.height) - logo_actual.height
    if (Object.values(logo_space).every(x => x < 0)) {
      return -logo_space.vertical
    } else {
      return 0
    }
  }

function adjustLegendItems(gd) {
  let visdata = gd.data.filter(trace => trace.visible === true || !(trace.hasOwnProperty('visible')));
  let legendItemTexts = $(gd).find('.legend .legendtext')
  visdata.forEach(trace => {
    let thisLegendItem = legendItemTexts.filter(function() {
    return $(this).text() === trace.name;
});
    if(thisLegendItem.length > 0) {
        let thisLegendWidth = thisLegendItem[0].getBBox().width
        trace.legendwidth = Math.round(thisLegendWidth*1.05)
    }
  })
}

function setVerticalLayout(eventdata, gd, legend_fontsize, plot_title, pie_chart, logo = undefined) {
  if ('width' in eventdata | 'autosize' in eventdata) {
    if ('rangeslider' in gd.layout.xaxis) {
      if (gd.layout.xaxis.rangeslider.visible == false) {
        gd.layout.xaxis.rangeslider.visible = true;
      }}
    let title_text = "<span>" +
      (plot_title[2] ? "<b>" : "" ) +
      plot_title[0] +
      (plot_title[2] ? "</b>" : "" ) +
      (plot_title[0].length == 0 ? "" : "<br>") +
      "<span style='font-size: 75%'>" + plot_title[1] + "</span></span>"
    let caption_text = gd.layout.annotations[0].text.replace(new RegExp("<br class = 'roboplotr-breaker'>", 'g'), " ")
    Plotly.relayout(gd, {'title.text': title_text, 'annotations[0].text': caption_text})
    let gdtitle = $(gd).find('g.g-gtitle')[0].getBBox().width;
    let titlespace = pie_chart ? $(gd).find('g.layer-above') : $(gd).find('.cartesianlayer > .xy > .gridlayer');
    if (titlespace.length > 0) {titlespace = titlespace[0].getBBox().width};
    let relayout_array = getVerticalLayout(gd, legend_fontsize, false, keys = ['legend.font.size','margin.t','margin.b','legend.y','yaxis.tickfont.size'], pie_chart = pie_chart, logo = logo)
    if(titlespace <= gdtitle) {
      title_text = "<span>" +
        (plot_title[2] ? "<b>" : "" ) +
        stringDivider(plot_title[0], Math.floor(titlespace/(gd.layout.title.font.size-8)), "<br class = 'roboplotr-breaker'>") +
        (plot_title[2] ? "</b>" : "" ) +
        "<br><span style='font-size: 75%'>" + plot_title[1] + "</span></span>"
        relayout_array["title.text"] = title_text;
    }
    adjustLegendItems(gd)
    Plotly.relayout(gd, relayout_array);
    let logo_width = calculateDisplayedImageSize(logo, $(gd).find('g.layer-above > g.imagelayer > image')[0].getBBox()).width
    relayout_array = getVerticalLayout(gd, legend_fontsize, false, keys = ['legend.font.size','margin.t','margin.b','yaxis.tickfont.size','images[0].sizey'], pie_chart = pie_chart, logo = logo)
    relayout_array = findCaptionSpace(gd, logo, pie_chart, relayout_array, titlespace);
    Plotly.relayout(gd, relayout_array);
    relayout_array = getVerticalLayout(gd, legend_fontsize, false, keys = ['legend.font.size','margin.t','margin.b','yaxis.tickfont.size','images[0].sizey'], pie_chart = pie_chart, logo = logo)
    relayout_array = findCaptionSpace(gd, logo, pie_chart, relayout_array, titlespace);
    adjustLegendItems(gd)
    Plotly.relayout(gd, relayout_array);
    Plotly.relayout(gd, getVerticalLayout(gd, legend_fontsize, false, keys = ['images[0].y', 'annotations[0].y', 'margin.t', 'margin.b','legend.y','images[0].sizey','yaxis.tickfont.size'], pie_chart = pie_chart, logo = logo));
    Plotly.relayout(gd, getVerticalLayout(gd, legend_fontsize, false, keys = ['images[0].y', 'annotations[0].y', 'legend.y', "margin.b", 'title.y'], pie_chart = pie_chart, logo = logo));
  }
}

function findCaptionSpace(gd, logo, pie_chart, relayout_array, titlespace) {
  let captionspace
  let gdcaption = $(gd).find('g.annotation')[0].getBBox().width;
  let caption_text = gd.layout.annotations[0].text.replace(new RegExp("<br class = 'roboplotr-breaker'>", 'g'), " ")
  if(pie_chart) {
      captionspace = titlespace
    } else {
      let plot_width = $(gd).find('.gridlayer')[0].getBBox().width
      let logo_width = calculateDisplayedImageSize(logo, $(gd).find('g.layer-above > g.imagelayer > image')[0].getBBox()).width
      captionspace = Math.max(plot_width - logo_width, Math.round(logo_width / 2))
    }
    relayout_array['annotations[0].text'] = stringDivider(caption_text, Math.floor(captionspace/(gd.layout.annotations[0].font.size/2)), "<br class = 'roboplotr-breaker'>")

    return relayout_array
}

function findShapeById(plotDiv, customId) {
  let shapes = plotDiv.layout.shapes || [];
  for (let i = 0; i < shapes.length; i++) {
    if (shapes[i].shapeId === customId) {
      return i;
    }
  }
  return null; // If no shape is found with the given identifier
}

function editShapes(gd, line_label) {
  let sareashape = findShapeById(gd, "shadearea")
  if(sareashape !== null) {
    gd.layout.shapes[sareashape].y0 = gd.layout.yaxis.range[0]
    gd.layout.shapes[sareashape].y1 = gd.layout.yaxis.range[1]
  }
  let zlineshape = findShapeById(gd, 'zeroline');
  if (zlineshape !== null) {
     gd.layout.shapes[zlineshape].x0 = gd.layout.xaxis.range[0]
  gd.layout.shapes[zlineshape].x1 = gd.layout.xaxis.range[1]
  }
  if(zlineshape !== null | sareashape !== null) {
     Plotly.redraw(gd)
  }
  if(zlineshape !== null) {
    gd.on('plotly_afterplot', function() {
    let yticks = $(gd).find('g.ytick text')
    let zeroline = $(gd).find('path.zl')
    if(line_label == 0) {
      if (zeroline.length > 0) {
      zeroline[0].style['stroke'] = gd.layout.shapes[zlineshape].line.color
      }
    } else {
    let label_translate
    if (zeroline.length > 0) { zeroline[0].style['stroke'] = gd.layout.yaxis.gridcolor };
    yticks.filter(function(d, i) {
    if(this.textContent.trim().replace(',','.').replace('\u2212','-').replace(' ','') == line_label) {
    label_translate = i.getAttribute('transform')
      let lines = $(gd).find('path.ygrid')
      lines.filter(function(d, i) {
      if(i.getAttribute('transform') == label_translate) {
        i.style['stroke'] = gd.layout.shapes[zlineshape].line.color
      }
      })
    }
    })
    }
})
  }
}

function stringDivider(str, width, spaceReplacer) {
  if (str.length>width) {
    let p=width
    for (;p>0 && str[p]!=' ';p--) {
    }
    if (p>0) {
      let left = str.substring(0, p);
      let right = str.substring(p+1);
      return left + spaceReplacer + stringDivider(right, width, spaceReplacer);
    } else {
      let sp = /( ){1,}/ig;
      return str.replace(sp,spaceReplacer)
    }
  }
  return str;
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
		let zline = findShapeById(gd, "zeroline")
		if(zline != null) {
		  var update = {
			'yaxis.range': [yMin,yMax], 'shapes[zline].x0': xRange[0]  // updates the end of the yaxis range
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

function plotlyRelayoutEventFunction(eventdata, gd, legend_fontsize, plot_title, rangeslider_sums, pie_chart, logo) {
  timerId = 0;
  setVerticalLayout(eventdata, gd, legend_fontsize, plot_title, pie_chart, logo);
	yrangeRelayout(eventdata, gd, timerId, rangeslider_sums);
};

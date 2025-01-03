function rangeSliderShowHide(e,t=!0){"rangeslider"in e.layout.xaxis&&e.layout.xaxis.rangeslider.visible!=t?Plotly.relayout(e,{showlegend:t,"xaxis.rangeslider.visible":t}):Plotly.relayout(e,{showlegend:t})}function rangeSliderHeight(e){let t=0;var i;return"rangeslider"in e.layout.xaxis&&1==e.layout.xaxis.rangeslider.visible&&(elslider_container=$(e).find(".rangeslider-bg")[0].getBBox().height,t=elslider_container+.1*e.layout.margin.t+.1*e.layout.margin.b,0<(i=$(e).find("g.xaxislayer-above")).length&&(i=$(e).find("g.xaxislayer-above")[0].getBBox().height,t+=i/10)),t}function findModeBarHeight(e){let t=5,i=$(e).find("div.modebar");return 0<i.length&&"none"!=i.css("display")&&(t=i[0].clientHeight),t}function getVerticalLayout(i,e,t,a,n,l=void 0,o=!1,g="auto"){var r=e=>/<br>/.test(e)?11:16;adjustLegendItems(i,o);var h={height:$(i).find("svg.main-svg")[0].height.animVal.value,width:$(i).find("svg.main-svg")[0].width.animVal.value},s=$(i).find("g.g-gtitle")[0].getBBox(),d=0<$(i).find("g.xy2").length,y=0<$(i).find("g.g-y2title").length;let u=rangeSliderHeight(i);var f=n?5:0;let x=$(i).find("g.xaxislayer-above");x=0<x.length?x[0].getBBox().height:f;o=findModeBarHeight(i);let c=5;d&&(p=$(i).find("g.overaxes-above > g.xy2-y")[0].getBBox(),v=y?$(i).find("g.g-y2title")[0].getBBox().width:0,c=p.width+5+v);var p=s.height+o+r(i.layout.title.text),v=$(i).find("g.annotation")[0].getBBox().height+5;let m=$(i).find("g.g-xtitle");m=0<m.length?1.3*m[0].getBBox().height:0;let B={width:0,height:0};null!=$(i).find("g.legend")[0]&&(b=$(i).find("g.legend")[0].getBBox(),B.height=b.height,B.width=b.width);let w=n?$(i).find(".pielayer"):$(i).find(".nsewdrag");0<w.length&&(w=w[0].getBBox()),"auto"==g&&(g="right"==i.layout.legend.position?w.width/h.width<.67||w.width/B.width<.75?"bottom":"right":.75<w.width/h.width||1.2<w.width/B.width?"right":"bottom");o="bottom"==g?"h":"v";null!=$(i).find("g.legend")[0]&&(k=$(i).find("g.legend")[0].getBBox(),B.height="right"==g?0:Math.ceil(k.height),B.width=k.width);var b="bottom"==(i.layout.legend.position=g)?0:1.02;let z=B.height+15+(v+x+m);n&&(w.height=h.height-z-p),0==w.height&&(w.height=1);var k=$(i).find("g.layer-above > g.imagelayer > image")[0].getBBox(),l=logoSpace(l,k,z,m,x,B,o);z+=l;let S=.05*h.height/w.height;i.layout.images[0].sizey=S;let M="right"==g?1:-((x+10+u+m)/w.height);if(M<-2||w.height<h.height/4){rangeSliderShowHide(i,!1),u=0,x=$(i).find("g.xaxislayer-above"),x=0<x.length?x[0].getBBox().height:f,z=15+v+x+m;let e=n?$(i).find(".pielayer"):$(i).find(".nsewdrag");0<e.length&&(e=e[0].getBBox()),n&&(e.height=h.height-z-p),0==e.height&&(e.height=1),S=.05*h.height/e.height,i.layout.images[0].sizey=S,M="right"==g?1:-((x+10+u+m)/e.height)}else if(w.height>h.height/1.5&0==i.layout.showlegend){rangeSliderShowHide(i,!0),u=rangeSliderHeight(i);let e=$(i).find("g.xaxislayer-above");e=0<e.length?e[0].getBBox().height:f;f=findModeBarHeight(i),f=s.height+f+r(i.layout.title.text),r=$(i).find("g.annotation")[0].getBBox().height+5;let t=$(i).find("g.g-xtitle");t=0<t.length?1.3*t[0].getBBox().height:0,B={width:0,height:0},null!=$(i).find("g.legend")[0]&&(B.height="bottom"==g?Math.ceil($(i).find("g.legend")[0].getBBox().height):0,B.width=$(i).find("g.legend")[0].getBBox().width),z=B.height+15+(r+e+t),w=n?$(i).find(".pielayer"):$(i).find(".nsewdrag"),0<w.length&&(w=w[0].getBBox()),n&&(w.height=h.height-z-f),0==w.height&&(w.height=1),k=$(i).find("g.layer-above > g.imagelayer > image")[0].getBBox(),S=.05*h.height/w.height,i.layout.images[0].sizey=S,M=-((e+10+u+t)/w.height)}w=n?$(i).find(".pielayer"):$(i).find(".nsewdrag"),0<w.length&&(w=w[0].getBBox()),n&&(w.height=h.height-z-p),0==w.height&&(w.height=1);let P=B.height>w.height/2?e.legend-2:e.legend;P=B.width>$(i).find("svg.main-svg")[0].width.animVal.value?P-2:P;let I=e.y,V=$(i).find("g.yaxislayer-above");0<V.length&&(h=[...$(i).find("g.ytick")].reduce((e,t)=>e+t.getBBox().height,0),I=w.height<=h?Math.floor(.8*i.layout.yaxis.tickfont.size):Math.min(Math.floor(1.5*i.layout.yaxis.tickfont.size),e.y),"container"==i.layout.annotations[0].xmod&&(e=V[0].getBBox().width,i.layout.annotations[0].xshift=-e));var _={"legend.orientation":o,"legend.x":b,"legend.y":M,"images[0].sizey":S,"margin.t":p,"margin.b":z,"margin.r":c,"legend.font.size":P,"yaxis.tickfont.size":I,"yaxis2.tickfont.size":I};return a.reduce(function(e,t){return t in _&&(e[t]=_[t]),e},{})}function calculateDisplayedImageSize(e,t){let i,a;return e<t.width/t.height?(a=t.height,i=a*e):(i=t.width,a=i/e),{width:Math.round(i),height:Math.round(a)}}function logoSpace(e,t,i,a,n,l,o){e=calculateDisplayedImageSize(e,t);let g=[];"bottom"==o&&l.width;return g.horizontal=t.width-l.width-e.width,g.vertical=i-(a+n+l.height)-e.height,Object.values(g).every(e=>e<0)?-g.vertical:0}function adjustLegendItems(e,t=!0){let n=$(e).find(".legend .legendtext");if(t){t=e.data.reduce((e,t)=>{let i=n.filter(function(){return $(this).text()===t.name});if(0<i.length){var a=i[0].getBBox().width;return Math.max(e,a)}return e},0),t=Math.round(1.02*t);e.layout.legend.entrywidth=t}else{let a=$(e).find(".legend .legendtext");e.data.forEach(e=>{let t=a.filter(function(){return $(this).text()===e.name});var i;0<t.length&&(i=t[0].getBBox().width,e.legendwidth=Math.round(1.1*i))})}}function setVerticalLayout(i,a,n,l,o,g=void 0,r=!1,h="auto"){if("width"in i|"autosize"in i){"rangeslider"in a.layout.xaxis&&0==a.layout.xaxis.rangeslider.visible&&(a.layout.xaxis.rangeslider.visible=!0);var s="<span>"+(l[2]?"<b>":"")+l[0]+(l[2]?"</b>":"")+(0==l[0].length?"":"<br>")+"<span style='font-size: 75%'>"+l[1]+"</span></span>",i=a.layout.annotations[0].text.replace(new RegExp("<br class = 'roboplotr-breaker'>","g")," ");Plotly.relayout(a,{"title.text":s,"annotations[0].text":i});i=$(a).find("g.g-gtitle")[0].getBBox().width;let e=o?$(a).find("g.layer-above"):$(a).find(".nsewdrag");0<e.length&&(e=e[0].getBBox().width);let t=getVerticalLayout(a,n,!1,keys=["legend.font.size","margin.t","margin.b","legend.orientation","legend.x","legend.y","yaxis.tickfont.size","yaxis2.tickfont.size"],o,g,r,h);e<=i&&(s="<span>"+(l[2]?"<b>":"")+stringDivider(l[0],Math.floor(e/(a.layout.title.font.size-8)),"<br class = 'roboplotr-breaker'>")+(l[2]?"</b>":"")+"<br><span style='font-size: 75%'>"+l[1]+"</span></span>",t["title.text"]=s),rangeSliderShowHide(a,!0),Plotly.relayout(a,t);calculateDisplayedImageSize(g,$(a).find("g.layer-above > g.imagelayer > image")[0].getBBox()).width;t=getVerticalLayout(a,n,!1,keys=["legend.font.size","legend.orientation","legend.x","legend.y","margin.t","margin.b","margin.r","yaxis.tickfont.size","yaxis2.tickfont.size","images[0].sizey"],o,g,r,h),t=findCaptionSpace(a,g,o,t,e),Plotly.relayout(a,t),t=getVerticalLayout(a,n,!1,keys=["legend.font.size","legend.orientation","legend.x","legend.y","margin.t","margin.b","margin.r","yaxis.tickfont.size","yaxis2.tickfont.size","images[0].sizey"],o,g,r,h),t=findCaptionSpace(a,g,o,t,e),Plotly.relayout(a,t),t=getVerticalLayout(a,n,!1,keys=["legend.font.size","margin.t","margin.b","legend.orientation","legend.x","legend.y","images[0].sizey","yaxis.tickfont.size","yaxis2.tickfont.size"],o,g,r,h),t=findCaptionSpace(a,g,o,t,e),Plotly.relayout(a,t),t=getVerticalLayout(a,n,!1,keys=["legend.font.size","margin.t","margin.b","legend.orientation","legend.x","legend.y","images[0].sizey","yaxis.tickfont.size","yaxis2.tickfont.size"],o,g,r,h),Plotly.relayout(a,t),setUpdatemenuPosition(a)}}function setUpdatemenuPosition(e){var t,i;0<$(e).find("g.updatemenu-container").length&&($(e).find(".nsewdrag")[0].getBBox(),$(e).find("g.updatemenu-container")[0].getBBox(),t="top"==e.layout.updatemenus[0].yanchor?.98:.02,i="left"==e.layout.updatemenus[0].xanchor?.02:.98,Plotly.relayout(e,{"updatemenus[0].y":t,"updatemenus[0].x":i}))}function setYPositions(i,a,n=!1){if("width"in i|"autosize"in i){var l=$(a).find("svg.main-svg")[0].height.animVal.value,o=(l-(21+findModeBarHeight(a)))/l;let e=n?$(a).find(".pielayer"):$(a).find(".nsewdrag");0<e.length&&(e=e[0].getBBox());var g=a.layout.margin.b,i=a.layout.margin.t;let t=l-e.height-a.layout.margin.t;g=Math.round(l-i-g);!0===n&&(n=$(a).find("svg.main-svg")[0].width.animVal.value,g>Math.round(e.height)&&2*n<l&&(t-=g-e.height,e.height=e.height+(g-e.height)));g=-(t/e.height);Plotly.relayout(a,{"images[0].y":g,"annotations[0].y":g,"title.y":o})}}function findCaptionSpace(e,t,i,a,n){let l;$(e).find("g.annotation")[0].getBBox().width;var o=e.layout.annotations[0].text.replace(new RegExp("<br class = 'roboplotr-breaker'>","g")," ");return l=i?n:(n=$(e).find(".nsewdrag")[0].getBBox().width,t=calculateDisplayedImageSize(t,$(e).find("g.layer-above > g.imagelayer > image")[0].getBBox()).width,Math.max(n-t,Math.round(t/2))),a["annotations[0].text"]=stringDivider(o,Math.floor(l/(e.layout.annotations[0].font.size/2)),"<br class = 'roboplotr-breaker'>"),a}function findAnnotationById(e,t){var i=e.layout.annotations||[];for(let e=0;e<i.length;e++)if(i[e].annotationId===t)return e;return null}function findShapeById(e,t){var i=e.layout.shapes||[];for(let e=0;e<i.length;e++)if(i[e].shapeId===t)return e;return null}function editShapes(a,n){var e=findShapeById(a,"shadearea");null!==e&&(a.layout.shapes[e].y0=a.layout.yaxis.range[0],a.layout.shapes[e].y1=a.layout.yaxis.range[1],1==a.layout.shapes[e].xnull&&(a.layout.shapes[e].x1=a.layout.xaxis.range[1]));var l=findShapeById(a,"zeroline");null!==l&&(a.layout.shapes[l].x0=a.layout.xaxis.range[0],a.layout.shapes[l].x1=a.layout.xaxis.range[1]),null!==l|null!==e&&Plotly.redraw(a),null!==l&&a.on("plotly_afterplot",function(){let e=$(a).find("g.ytick text"),t=$(a).find("path.zl");if(0==n)0<t.length&&(t[0].style.stroke=a.layout.shapes[l].line.color);else{let i;0<t.length&&(t[0].style.stroke=a.layout.yaxis.gridcolor),e.filter(function(e,t){if(this.textContent.trim().replace(",",".").replace("−","-").replace(" ","")==n){i=t.getAttribute("transform");let e=$(a).find("path.ygrid");e.filter(function(e,t){t.getAttribute("transform")==i&&(t.style.stroke=a.layout.shapes[l].line.color)})}})}})}function stringDivider(t,i,a){if(t.length>i){let e=i;for(;0<e&&" "!=t[e];e--);if(0<e)return t.substring(0,e)+a+stringDivider(t.substring(e+1),i,a);i=/( ){1,}/gi;return t.replace(i,a)}return t}function yrangeRelayout(i,o,a,n){if("[object Array]"===Object.prototype.toString.call(i["xaxis.range"])|"xaxis.range[0]"in i|"xaxis.autorange"in i){var g=o.layout.xaxis.range,r=(o.layout.yaxis.range,[]);let l=[];var h=[];if(o.data.filter(e=>!0===e.visible||!e.hasOwnProperty("visible")).forEach(e=>{var t=e.y.length;h.push(e.type);for(var i=0;i<t;i++){var a=e.x[i],n=e.y[i];(a>=g[0]&&a<=g[1]||"category"==o.layout.xaxis.type)&&(r.push(n),"bar"==e.type&&(n={date:a,val:n},l.push(n)))}}),1==n){var s={};l.forEach(function(e){s.hasOwnProperty(e.date)&&0<e.val?s[e.date]=s[e.date]+e.val:0<e.val&&(s[e.date]=e.val)});var d,y={};for(d in l.forEach(function(e){y.hasOwnProperty(e.date)&&e.val<=0?y[e.date]=y[e.date]+e.val:e.val<=0&&(y[e.date]=e.val)}),l=[{val:0}],y)l.push({date:d,val:y[d]});for(d in s)l.push({date:d,val:s[d]});r=r.concat(l.map(e=>e.val))}let e,t;"xaxis.autorange"in i?(e=o._init_yrange.x1,t=o._init_yrange.x0):(e=Math.max(...r),t=Math.min(...r),f=.04*Math.abs(e-t),e+=f,u=h.includes("bar")&&r.every(e=>0<=e),1!=n&&0==u&&(t-=f));var n=findShapeById(o,"zeroline"),u=findShapeById(o,"shadearea"),f={"yaxis.range":[t,e]};null!=n&&("xaxis.autorange"in i?(f["shapes["+n+"].x0"]=o._init_xrange.x0,f["shapes["+n+"].x1"]=o._init_xrange.x1):(f["shapes["+n+"].x0"]=g[0],f["shapes["+n+"].x1"]=g[1])),null!=u&&(1==o.layout.shapes[u].xnull&&("xaxis.autorange"in i?f["shapes["+u+"].x1"]=o._init_xrange.x1:f["shapes["+u+"].x1"]=g[1]),f["shapes["+u+"].y0"]=t,f["shapes["+u+"].y1"]=e),Plotly.relayout(o,f),0<=a&&window.clearTimeout(a),a=window.setTimeout(function(){a=-1},800)}}function plotlyRelayoutEventFunction(e,t,i,a,n,l,o,g,r){timerId=0,setVerticalLayout(e,t,i,a,l,o,g,r),setYPositions(e,t,l),yrangeRelayout(e,t,timerId,n)}

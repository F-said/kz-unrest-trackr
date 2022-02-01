maps_path <- "data/maps/"
KZ_lat <- 48.0196
KZ_long <- 66.9237

json_pos <- 1
coor_pos <- 2
lat_pos <- 1
long_pos <- 2
geo_maps <- dict()
geo_maps$set("Kazakhstan", list("kz_1.json", list(KZ_lat, KZ_long)))$
  set("Russia", list("ru_2.json", list(KZ_lat+8, KZ_long-10)))$
  set("Kyrgyzstan", list("kg_1.json", list(KZ_lat-6, KZ_long+8)))$
  set("Tajikistan", list("tj_1.json", list(KZ_lat-7, KZ_long-3)))$
  set("Ukraine", list("ua_1.json", list(0,0)))$
  set("Armenia", list("am_1.json", list(0,0)))

kz_on_id <- "inter_on"
kz_off_id <- "inter_off"
group_id <- "central_asia"

path <- paste(maps_path,geo_maps$get("Kazakhstan")[[json_pos]],sep="")
KZ_map <- geojson_read(path, what="sp")
KZ_map_shape <- geojson_sf(path)

# initialize map colors and annotations
name_color <- "navy"
default_color <- "#808080"

inter_on_color <- "#69b3a280"
inter_off_color <- "#bf40bf80"
name_size <- 5.0

# pull legend data
legend_dat <- read.csv('data/legend-info.csv', fileEncoding='UTF-8-BOM')

legend_dat_html <- data.frame(legend_dat)

legend_dat_html$label <- gsub("-", "", legend_dat_html$label)
legend_dat_html$label <- gsub(" ", "", legend_dat_html$label)
legend_dat_html$label <- factor(legend_dat_html$label, levels=legend_dat_html$label)

# generate palette to align to events using legend data
pall <- colorFactor(palette=legend_dat_html$color, levels=legend_dat_html$label)

cluster_script <-
  paste0(
    "function(cluster) {
      const groups= [",paste("'",levels(legend_dat_html$label),"'",sep="",collapse=","),"];
      const colors= {
        groups: [",paste("'",legend_dat_html$color,"'",sep="",collapse=","),"],
        center:'#ddd',
        text:'black'
      };
      
      const markers= cluster.getAllChildMarkers();

      const proportions= groups.map(
        group => markers.filter(marker => marker.options.group === group).length / markers.length);

      function sum(arr, first= 0, last) {
        return arr.slice(first, last).reduce((total, curr) => total+curr, 0);
      }
      
      const cumulativeProportions= proportions.map((val, i, arr) => sum(arr, 0, i+1));
      cumulativeProportions.unshift(0);

      const width = 2*Math.sqrt(markers.length);
      const radius = 15+width/2;

      const arcs= cumulativeProportions.map((prop, i) => { 
        return {
          x   :  radius*Math.sin(2*Math.PI*prop),
          y   : -radius*Math.cos(2*Math.PI*prop),
          long: proportions[i-1] >.5 ? 1 : 0
        }
      });
      
      const paths = proportions.map((prop, i) => {
        if (prop === 0) return '';
        else if (prop === 1) return `<circle cx='0' cy='0' r='${radius}' fill='none' stroke='${colors.groups[i]}' stroke-width='${width}' stroke-alignment='center' stroke-linecap='butt' />`;
        else return `<path d='M ${arcs[i].x} ${arcs[i].y} A ${radius} ${radius} 0 ${arcs[i+1].long} 1 ${arcs[i+1].x} ${arcs[i+1].y}' fill='none' stroke='${colors.groups[i]}' stroke-width='${width}' stroke-alignment='center' stroke-linecap='butt' />`
      });

      return new L.DivIcon({
        html: 
          `<svg width='60' height='60' viewBox='-30 -30 60 60' style='width: 60px; height: 60px; position: relative; top: -24px; left: -24px;' >
            <circle cx='0' cy='0' r='15' stroke='none' fill='${colors.center}' />
            <text x='0' y='0' dominant-baseline='central' text-anchor='middle' fill='${colors.text}' font-size='15'>${markers.length}</text>
          ${paths.join('')}
          </svg>`,
        className: 'marker-cluster'
      });
    }")
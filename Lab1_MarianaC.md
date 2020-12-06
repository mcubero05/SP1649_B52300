Habits
================

``` r
library(spDataLarge)
#Librerías requeridas
library(sf)
```

    ## Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1

``` r
library(rgeos)
```

    ## Loading required package: sp

    ## rgeos version: 0.5-3, (SVN revision 634)
    ##  GEOS runtime version: 3.8.0-CAPI-1.13.1 
    ##  Linking to sp version: 1.4-2 
    ##  Polygon checking: TRUE

``` r
library(rnaturalearth)
library(rnaturalearthdata)
library(stars)
```

    ## Loading required package: abind

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:rgeos':
    ## 
    ##     intersect, setdiff, union

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(raster)
```

    ## 
    ## Attaching package: 'raster'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2)
```

# Ejercicio 1.3

1.  Read the shapefile storms\_xyz\_feature from the shape directory in
    the sf package

<!-- end list -->

``` r
dataset <- system.file("shape", package="sf") %>% st_read("storms_xyz_feature") %>% st_zm()
```

    ## Reading layer `storms_xyz_feature' from data source `C:\Users\maric\Documents\R\win-library\4.0\sf\shape' using driver `ESRI Shapefile'
    ## Simple feature collection with 71 features and 1 field
    ## geometry type:  LINESTRING
    ## dimension:      XYZ
    ## bbox:           xmin: -102.2 ymin: 8.3 xmax: 0 ymax: 59.5
    ## z_range:        zmin: 924 zmax: 1017
    ## CRS:            NA

``` r
plot(dataset)
```

![](Lab1_MarianaC_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

2.  Copy this file to another directory on your computer, and read it
    from there (note: a shapefile consists of more than one file\!)

<!-- end list -->

``` r
#setwd("C:/Users/maric/OneDrive/maestria/Geoestadistica/SP1649-II20/clase2/Laboratorio1")
#sf::st_write(dataset, "storms_xyz_feature/storms_xyz_feature.shp")
```

3.  How many features does this dataset contain?

<!-- end list -->

``` r
#dataset$geometry
```

Hay 71 features

4.  Plot the dataset, with axes = TRUE (hint: before plotting, pipe
    through st\_zm to drop Z and M coordinates; more about this in
    chapter 3).

<!-- end list -->

``` r
par(mfrow=c(1,2))
plot(dataset, axes = T, main ="Sin set_crs")
```

![](Lab1_MarianaC_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
plot(dataset %>% st_set_crs(4326), axes = T, main= "Set_crs")
```

![](Lab1_MarianaC_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

5.  Before plotting, pipe the dataset through st\_set\_crs(4326). What
    is different in the plot obtained? La diferencia está en las
    unidades de medida el segundo ya entiende los valores de los ejes
    como coordenadas.

# Ejericio 2.6

# Punto 1

``` r
cart2pol <- function(x, y)
{
  r <- sqrt(x^2 + y^2)
  t <- atan(y/x)
  
  c(r,t)
}

# (c(10,2),c(-10,-2),c(10,-2),c(0,10))


polares <- rbind(cart2pol(10,2),
cart2pol(-10,-2),
cart2pol(10,-2),
cart2pol(0,10))
polares <- data.frame(polares)
names(polares) <- c("r","t")
polares
```

    ##          r          t
    ## 1 10.19804  0.1973956
    ## 2 10.19804  0.1973956
    ## 3 10.19804 -0.1973956
    ## 4 10.00000  1.5707963

# Punto 2

``` r
pol2car <- function(r, t)
{
  x <- r*cos(t)
  y <- r*sin(t)
  
  c(x,y)
}

matrix(round(pol2car(polares$r, polares$t),0), nrow=4, ncol=2)
```

    ##      [,1] [,2]
    ## [1,]   10    2
    ## [2,]   10    2
    ## [3,]   10   -2
    ## [4,]    0   10

# Punto 3

``` r
r=6371

polares.3 <- rbind(cart2pol(10,10),
                 cart2pol(11,10),
                 cart2pol(10,80),
                 cart2pol(11,80),
                 cart2pol(10,10),
                 cart2pol(10,11),
                 cart2pol(10,80),
                 cart2pol(10,81))
polares.3 <- data.frame(polares.3)
# 1
distancias = rbind(
acos(sin(polares.3[1,2])*sin(polares.3[2,2]) + cos(polares.3[1,2])*cos(polares.3[2,2])*cos(abs(polares.3[1,1]-polares.3[2,1]))),
# 2
acos(sin(polares.3[3,2])*sin(polares.3[4,2]) + cos(polares.3[3,2])*cos(polares.3[4,2])*cos(abs(polares.3[3,1]-polares.3[4,1]))),
# 3
acos(sin(polares.3[5,2])*sin(polares.3[6,2]) + cos(polares.3[5,2])*cos(polares.3[6,2])*cos(abs(polares.3[5,1]-polares.3[6,1]))),
# 4 
acos(sin(polares.3[7,2])*sin(polares.3[8,2]) + cos(polares.3[7,2])*cos(polares.3[8,2])*cos(abs(polares.3[7,1]-polares.3[8,1])))
)

data.frame(distancias) 
```

    ##   distancias
    ## 1  0.5203428
    ## 2  0.0208980
    ## 3  0.4958222
    ## 4  0.1174568

``` r
row.names(distancias) = c("Dist1", "Dist2","Dist3","Dist4")
distancias
```

    ##            [,1]
    ## Dist1 0.5203428
    ## Dist2 0.0208980
    ## Dist3 0.4958222
    ## Dist4 0.1174568

\#Punto 4.4 Parte 1

``` r
library(stars)
tif = system.file("tif/L7_ETMs.tif", package = "stars")
x = read_stars(tif)
bandas = split(x, "band")
NIR = bandas$X4
R = bandas$X3

ndvi = (NIR-R)/(NIR+R)
plot(ndvi)
```

![](Lab1_MarianaC_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
#write_stars(ndvi, tf)
```

Parte 2

``` r
ndvi = function(x) (x[4]-x[3])/(x[4]+x[3])
b<-st_apply(x, c("x", "y"), ndvi)
tf = tempfile(fileext=".tif")
#write_stars(b, tf)
```

Parte 3

``` r
x.st=st_transform(x,4326)
#print(x.st)

#plot(c,axes=TRUE, border=NA)
```

Parte 4

``` r
c_warp = st_warp(x, crs=4326)
plot(c_warp)
```

![](Lab1_MarianaC_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

# Punto 6.6

``` r
nc <- system.file("gpkg/nc.gpkg", package="sf") %>%
  read_sf() %>%
  st_transform(32119)

nc$State = "North Carolina"

st_union(nc)
```

    ## Geometry set for 1 feature 
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: 123829.8 ymin: 14740.06 xmax: 930518.6 ymax: 318255.5
    ## projected CRS:  NAD83 / North Carolina

    ## MULTIPOLYGON (((705428.2 49242.97, 705860.3 274...

# Punto 8.7 (lovelace)

1.  Create a map showing the geographic distribution of the Human
    Development Index (HDI) across Africa with base graphics (hint: use
    plot()) and tmap packages (hint: use tm\_shape(africa)

R base es muy fácil de usar y permite hacer pruebas rápido, pero es muy
tieso para personalizar los mapas.

``` r
africa = world %>% 
  filter(continent == "Africa", !is.na(iso_a2)) %>% 
  left_join(worldbank_df, by = "iso_a2") %>% 
  dplyr::select(name, subregion, gdpPercap, HDI, pop_growth) %>% 
  st_transform("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25")

plot(africa["HDI"])
```

![](Lab1_MarianaC_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Con tm\_map se puede presentar más bonito el mapa y con casi igual
facilidad que con base, además permite mapas interactivos muy
fácilmente.

``` r
tm_shape(africa) + tm_polygons("HDI")
```

![](Lab1_MarianaC_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Otra opción es con ggplot2, ciertamente es más trabajoso,pero tiene
muchas más opciones para personalizar. Otras opciones como leaflet y
mapview son tan fáciles de usar como son de tiesas, son útiles para
mapas interactivos especialmente.

2.  Extend the map of Africa created with tmap for the previous exercise
    so the legend has three bins: “High” (HDI above 0.7), “Medium” (HDI
    between 0.55 and 0.7) and “Low” (HDI below 0.55). Bonus: improve the
    map aesthetics, for example by changing the legend title, class
    labels and color palette.

<!-- end list -->

``` r
tm1 = tm_shape(africa) +
  tm_polygons(
  col = "HDI",
  title = "Human Development Index",
  breaks = c(0, 0.55, 0.7, 0.8),
  labels = c("Low", "Medium", "High"),
  palette = "magma")
tm1
```

![](Lab1_MarianaC_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

3.  Represent africa’s subregions on the map. Change the default color
    palette and legend title. Next, combine this map and the map created
    in the previous exercise into a single plot.

<!-- end list -->

``` r
tm2 = tm_shape(africa) +
  tm_polygons(col = "subregion",
              title = "Subregion", 
              palette = "Set2")
tmap_arrange(tm2, tm1)
```

![](Lab1_MarianaC_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

4.  Create a land cover map of the Zion National Park.

<!-- end list -->

  - Change the default colors to match your perception of the land cover
    categories
  - Add a scale bar and north arrow and change the position of both to
    improve the map’s aesthetic appeal
  - Bonus: Add an inset map of the Zion National Park’s location in the
    context of the Utah state. (Hint: an object representing Utah can be
    subsetted from the us\_states dataset.)

<!-- end list -->

``` r
library(sf)
library(raster)
library(dplyr)
library(spData)
library(tmap)
library(leaflet)
library(cartogram)
zion = st_read((system.file("vector/zion.gpkg", package = "spDataLarge")))
```

    ## Reading layer `zion' from data source `C:\Users\maric\Documents\R\win-library\4.0\spDataLarge\vector\zion.gpkg' using driver `GPKG'
    ## Simple feature collection with 1 feature and 11 fields
    ## geometry type:  POLYGON
    ## dimension:      XY
    ## bbox:           xmin: 302903.1 ymin: 4112244 xmax: 334735.5 ymax: 4153087
    ## projected CRS:  UTM Zone 12, Northern Hemisphere

``` r
data(nlcd, package = "spDataLarge")

utah = dplyr::filter(us_states, NAME == "Utah")
zion_bbox = st_as_sfc(st_bbox(nlcd))

im = tm_shape(utah) +
  tm_polygons(lwd = 3, border.col = "black") + 
  tm_shape(zion_bbox) +
  tm_polygons(col = "green", lwd = 1) +
  tm_layout(title = "UTAH", title.size = 2, title.position = c("center", "center")) +
  tm_layout(frame = FALSE, bg.color = NA)
im
```

![](Lab1_MarianaC_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

5.  Create facet maps of countries in Eastern Africa:

<!-- end list -->

  - With one facet showing HDI and the other representing population
    growth (hint: using variables HDI and pop\_growth respectively)
  - With a ‘small multiple’ per country

<!-- end list -->

``` r
eastern_africa = filter(africa, subregion == "Eastern Africa")
tm_shape(eastern_africa) +
  tm_polygons(col = c("HDI", "pop_growth")) +
  qtm(africa, fill = NULL)
```

![](Lab1_MarianaC_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
tm_shape(eastern_africa) +
  tm_polygons("pop_growth", style = "jenks", palette = "viridis") +
  tm_facets(by = "name", drop.NA.facets = TRUE)
```

![](Lab1_MarianaC_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

# 6 Building on the previous facet map examples, create animated maps of East Africa:

  - Showing first the spatial distribution of HDI scores then population
    growth
  - Showing each country in order

# Create an interactive map of Africa

``` r
tmap_mode("view")
```

    ## tmap mode set to interactive viewing

``` r
tm_shape(eastern_africa) +
  tm_polygons("pop_growth") +
  tm_scale_bar() # bonus scale bar
```

    ## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.

<!--html_preserve-->

<div id="htmlwidget-c35b248fbfe6576a9d9a" class="leaflet html-widget" style="width:672px;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-c35b248fbfe6576a9d9a">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"createMapPane","args":["tmap401",401]},{"method":"addProviderTiles","args":["Esri.WorldGrayCanvas",null,"Esri.WorldGrayCanvas",{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"pane":"tilePane"}]},{"method":"addProviderTiles","args":["OpenStreetMap",null,"OpenStreetMap",{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"pane":"tilePane"}]},{"method":"addProviderTiles","args":["Esri.WorldTopoMap",null,"Esri.WorldTopoMap",{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"pane":"tilePane"}]},{"method":"addPolygons","args":[[[[{"lng":[33.9037111971045,34.07262,37.6986899999999,37.7669,39.20222,38.74054,38.7997700000001,39.44,39.4700000000001,39.19469,39.25203,39.1865200000001,39.5357400000001,39.9496,40.3165862291108,40.31659,39.521,38.4275565935878,37.82764,37.47129,36.7751509946228,36.5140816586843,35.312397902169,34.5599890479994,34.28,33.9408377240965,33.7397200000001,32.7593754412213,32.1918648617919,31.5563480974665,31.1577513369501,30.7400097314221,30.7400154965518,30.1999967791017,29.62003217949,29.4199927100882,29.5199866065729,29.3399975929003,29.7535124040999,30.11632,30.50554,30.7522400000001,30.74301,30.52766,30.4696736457612,30.46967,30.7583089535831,30.8161348813177,30.4191048520192,30.7698600000001,31.8661700000001,33.9037111971045],"lat":[-0.94999999999952,-1.05981999999978,-3.0969899999995,-3.67711999999945,-4.67677000000032,-5.90894999999988,-6.47566,-6.83999999999994,-7.09999999999996,-7.70390000000006,-8.00781000000007,-8.48550999999984,-9.11237000000013,-10.0984000000006,-10.3170977528174,-10.3170999999999,-10.8968800000001,-11.2852023250816,-11.2687899999998,-11.5687600000001,-11.5945374487813,-11.7209380021671,-11.4391464168795,-11.5200200334156,-10.1599999999998,-9.69367384198043,-9.41714999999955,-9.23059905358901,-8.93035898197308,-8.76204884199826,-8.59457874731718,-8.34000593035405,-8.34000741947069,-7.07998097089848,-6.52001515058319,-5.93999887453942,-5.41997893638628,-4.49998341229434,-4.45238941815333,-4.09012000000008,-3.56858000000006,-3.35931000000007,-3.03430999999938,-2.80761999999986,-2.41385475710109,-2.41383,-2.28725025798838,-1.69891407634556,-1.1346591121508,-1.01455000000005,-1.02736000000001,-0.94999999999952]}]],[[{"lng":[41.58513,40.993,40.98105,41.855083092644,42.12861,42.76967,43.66087,44.9636,47.78942,48.4867358742269,48.9381295102964,48.938232863161,48.9384912453225,48.9420052427183,48.9482047585097,48.9482047585099,49.26776,49.72862,50.25878,50.73202,51.1112,51.13387,51.04153,51.04531,50.83418,50.55239,50.07092,49.4527,48.59455,47.74079,46.56476,45.56399,44.06815,43.13597,42.04157,41.81095,41.58513],"lat":[-1.6832500000002,-0.858290000000163,2.78452000000011,3.91891192048376,4.23412999999991,4.25259000000017,4.95754999999995,5.00162000000004,8.00300000000016,8.83762624758973,9.45174896894627,9.97350006758193,10.9823273787832,11.3942660587987,11.410617281698,11.410617281698,11.43033,11.5789,11.67957,12.0219000000002,12.0246399999998,11.7481500000002,11.1665100000001,10.6409000000002,10.2797199999994,9.19873999999999,8.08173000000019,6.80466000000015,5.33910999999971,4.21940000000015,2.85528999999983,2.04575999999946,1.05282999999993,0.292200000000048,-0.919160000000231,-1.44647000000012,-1.6832500000002]}]],[[{"lng":[39.20222,37.7669,37.6986899999999,34.07262,33.9037111971045,33.8935689696669,34.18,34.6721,35.03599,34.59607,34.47913,34.005,34.6201962678539,35.298007118233,35.8174476623535,35.8174476623535,36.1590786328556,36.8550932380081,38.120915,38.43697,38.67114,38.89251,39.5593842587659,39.85494,40.76848,41.1718,41.855083092644,40.98105,40.993,41.58513,40.88477,40.63785,40.26304,40.12119,39.80006,39.60489,39.20222],"lat":[-4.67677000000032,-3.67711999999945,-3.0969899999995,-1.05981999999978,-0.94999999999952,0.109813537861806,0.515000000000006,1.17694,1.90583999999997,3.05374000000034,3.55560000000022,4.24988494736234,4.84712274208211,5.5060000000001,5.33823208279071,4.77696566346184,4.4478641276728,4.4478641276728,3.59860500000003,3.58850999999996,3.61606999999994,3.50073999999981,3.42206000000053,3.83879000000044,4.25701999999984,3.91909000000011,3.91891192048376,2.78452000000011,-0.858290000000163,-1.6832500000002,-2.08254999999989,-2.49979000000018,-2.57309000000051,-3.2776800000002,-3.68116000000003,-4.34653000000029,-4.67677000000032]}]],[[{"lng":[31.1914091326213,30.6598653500671,30.3228833350918,29.839036899543,29.432188348109,28.7946562029242,28.0213700701086,27.7272278175033,27.7247473487533,27.2965047543505,26.1647908871585,25.8503914730947,25.6491634457502,25.264225701608,26.3819352556489,26.7067733090356,27.0444271176307,27.5982434425028,28.4679061215427,28.8258687680285,28.9474634132113,29.5168343442031,30.2742558123051,30.3389547055345,31.1730639991577,31.6364982439512,31.8520406430406,32.3282389666102,32.8476387875758,32.8498608741644,32.6548856951271,32.6119942563249,32.7727079607526,32.6597432797626,32.5086930681734,32.244988234188,31.1914091326213],"lat":[-22.2515096981722,-22.1515674781197,-22.2716118303337,-22.1022164852806,-22.0913127580673,-21.6394540341074,-21.4859750302009,-20.8518018531154,-20.4990585262905,-20.3915198706908,-19.2930856258949,-18.7144129370907,-18.5360258928189,-17.7365398088312,-17.8460421688585,-17.9612289364363,-17.9380262183375,-17.2908305803141,-16.468400160389,-16.3897486304404,-16.0430514461939,-15.6446778296568,-15.5077869605151,-15.88083912523,-15.8609436987981,-16.0719902482778,-16.319417006091,-16.3920740698937,-16.7133981258845,-17.9790573055772,-18.6720899390439,-19.4193828264162,-19.7155921363131,-20.3042900529826,-20.3952922502481,-21.1164885393136,-22.2515096981722]}]],[[{"lng":[30.7400097314221,31.1577513369501,31.5563480974665,32.1918648617919,32.7593754412213,33.2313879737753,33.4856876970836,33.3153104998173,33.1142891782019,33.3064221534631,32.9917643572379,32.6881653175231,33.2140246925252,30.1794812354818,30.2742558123051,29.5168343442031,28.9474634132113,28.8258687680285,28.4679061215427,27.5982434425028,27.0444271176307,26.7067733090356,26.3819352556489,25.264225701608,25.0844433936646,25.0769503109823,24.6823490740015,24.0338615251708,23.2150484555061,22.5624784685243,21.8878426449539,21.9338863461259,24.0161365088947,23.9309220720454,24.0799052263428,23.9041536801182,24.0178935075926,23.9122152035557,24.257155389104,24.314516228948,24.783169793403,25.4181181169732,25.7523096046047,26.5530875993996,27.1644197934125,27.3887988624238,28.15510867688,28.523561639121,28.9342859229768,29.6996138852195,29.6160014177712,29.3415478858691,28.6424174333924,28.3722530453704,28.4960697771418,28.6736816749289,28.4498710466728,28.7348665707625,29.0029122250605,30.3460860531908,30.7400097314221],"lat":[-8.34000593035405,-8.59457874731718,-8.76204884199826,-8.93035898197308,-9.23059905358901,-9.67672169356442,-10.5255587703907,-10.7965499813296,-11.6071981746928,-12.4357780900607,-12.7838705379785,-13.7128577612893,-13.9718600399363,-14.7960991349914,-15.5077869605151,-15.6446778296568,-16.0430514461939,-16.3897486304404,-16.468400160389,-17.2908305803141,-17.9380262183375,-17.9612289364363,-17.8460421688585,-17.7365398088312,-17.6618156877379,-17.5788233374766,-17.3534107398196,-17.2958431942464,-17.5231161434662,-16.8984514299222,-16.0803101538769,-12.8984371883689,-12.9110462378484,-12.5658476701392,-12.1912968888866,-11.7222815894063,-11.2372982723473,-10.9268262671378,-10.9519926896635,-11.2628264298988,-11.2386935360188,-11.3309359676598,-11.7849651017761,-11.9244397925321,-11.6087484676612,-12.1327474911006,-12.2724805640176,-12.6986044246971,-13.2489584286049,-13.2572266577718,-12.1788945451371,-12.3607439103724,-11.9715686987823,-11.7936467424016,-10.7898837215639,-9.60592498132493,-9.16491830814593,-8.52655934004487,-8.40703175215326,-8.23825652428861,-8.34000593035405]}]],[[{"lng":[32.7593754412213,33.7397200000001,33.9408377240965,34.28,34.5599890479994,34.280006137842,34.5599890479994,34.9071513201362,35.267956170398,35.6868453305559,35.7719047381084,35.3390629412316,35.0338102556835,34.3812919451341,34.3072912940921,34.5176660499523,34.4596334164885,34.0648254737786,33.7897001482567,33.2140246925252,32.6881653175231,32.9917643572379,33.3064221534631,33.1142891782019,33.3153104998173,33.4856876970836,33.2313879737753,32.7593754412213],"lat":[-9.23059905358901,-9.41714999999955,-9.69367384198043,-10.1599999999998,-11.5200200334156,-12.2800253231325,-13.5799976538667,-13.5654248999604,-13.8878341610299,-14.6110458309544,-15.8968588192406,-16.1074402808302,-16.8012997372127,-16.1835596655961,-15.4786414527026,-15.0137085913727,-14.6130095353814,-14.3599500464486,-14.4518307430629,-13.9718600399363,-13.7128577612893,-12.7838705379785,-12.4357780900607,-11.6071981746928,-10.7965499813296,-10.5255587703907,-9.67672169356442,-9.23059905358901]}]],[[{"lng":[34.5599890479994,35.312397902169,36.5140816586843,36.7751509946228,37.47129,37.82764,38.4275565935878,39.521,40.31659,40.3165862291108,40.3165885760172,40.478387485523,40.4372530454187,40.5608113950286,40.5996203956798,40.775475294769,40.4772506040126,40.0892639503652,39.4525586280971,38.5383508644215,37.4111328468389,36.2812793312094,35.8964966163641,35.1983996925331,34.78638349787,34.7018925310728,35.1761271502154,35.3734277687057,35.3858482537054,35.5625455363691,35.5339347674043,35.3717741228724,35.6074703305556,35.4587455584196,35.0407348976107,34.2158240089355,33.013210076639,32.5746321957779,32.6603633969501,32.9159550310657,32.8301204770289,32.0716654802811,31.985779249812,31.8377779477281,31.7524084815819,31.9305888201242,31.6703979835347,31.1914091326213,32.244988234188,32.5086930681734,32.6597432797626,32.7727079607526,32.6119942563249,32.6548856951271,32.8498608741644,32.8476387875758,32.3282389666102,31.8520406430406,31.6364982439512,31.1730639991577,30.3389547055345,30.2742558123051,30.1794812354818,33.2140246925252,33.7897001482567,34.0648254737786,34.4596334164885,34.5176660499523,34.3072912940921,34.3812919451341,35.0338102556835,35.3390629412316,35.7719047381084,35.6868453305559,35.267956170398,34.9071513201362,34.5599890479994,34.280006137842,34.5599890479994],"lat":[-11.5200200334156,-11.4391464168795,-11.7209380021671,-11.5945374487813,-11.5687600000001,-11.2687899999998,-11.2852023250816,-10.8968800000001,-10.3170999999999,-10.3170977528174,-10.3170960425258,-10.7654407690899,-11.7617107072449,-12.6391765275609,-14.2019751929318,-14.6917644181943,-15.406294447494,-16.1007740210645,-16.7208912085672,-17.1010230445064,-17.586368096591,-18.6596875952932,-18.8422604305813,-19.5528113745937,-19.7840117326676,-20.497043145431,-21.2543612606687,-21.8408370907486,-22.14,-22.0900000000002,-23.0707878557272,-23.5353589820318,-23.7065630022146,-24.1226099585968,-24.4783505184933,-24.8163143856823,-25.3575733375077,-25.7273182105557,-26.1485844865995,-26.2158672014436,-26.7421916643359,-26.7338200823048,-26.29177988048,-25.8433318010512,-25.4842839494874,-24.3694165992224,-23.6589690080741,-22.2515096981722,-21.1164885393136,-20.3952922502481,-20.3042900529826,-19.7155921363131,-19.4193828264162,-18.6720899390439,-17.9790573055772,-16.7133981258845,-16.3920740698937,-16.319417006091,-16.0719902482778,-15.8609436987981,-15.88083912523,-15.5077869605151,-14.7960991349914,-13.9718600399363,-14.4518307430629,-14.3599500464486,-14.6130095353814,-15.0137085913727,-15.4786414527026,-16.1835596655961,-16.8012997372127,-16.1074402808302,-15.8968588192406,-14.6110458309544,-13.8878341610299,-13.5654248999604,-13.5799976538667,-12.2800253231325,-11.5200200334156]}]],[[{"lng":[30.4696736457612,30.52766,30.74301,30.7522400000001,30.50554,30.11632,29.7535124040999,29.3399975929003,29.2763839047491,29.0249263852168,29.6321761410786,29.9383590024079,30.4696736457612],"lat":[-2.41385475710109,-2.80761999999986,-3.03430999999938,-3.35931000000007,-3.56858000000006,-4.09012000000008,-4.45238941815333,-4.49998341229434,-3.29390715903389,-2.83925790773017,-2.91785776124591,-2.3484868302543,-2.41385475710109]}]],[[{"lng":[49.5435189145958,49.8089807472791,50.0565108579572,50.2174312681141,50.4765368996255,50.377111443896,50.2002746925932,49.8606055031387,49.6726066424609,49.8633443540502,49.7745642433727,49.4986120949341,49.4356185239703,49.0417924334739,48.548540887248,47.9307491391987,47.5477234230513,47.0957613462266,46.2824776548171,45.4095076841105,44.8335738462176,44.0397204933498,43.7637683449112,43.6977775408745,43.3456543312376,43.254187046081,43.4332975604046,43.8936828956929,43.8963700701721,44.3743253924397,44.4643974139244,44.2324219093662,44.0429761085842,43.9630843442609,44.3124687029863,44.4465173683514,44.9449365578065,45.502731967965,45.8729936053363,46.3122432798172,46.8821826515643,47.7051298358124,48.0052148781313,47.8690474790422,48.2938277524814,48.8450602557388,48.863508742067,49.1946513201933,49.5435189145958],"lat":[-12.4698328589409,-12.8952849259995,-13.5557614071217,-14.7587887508766,-15.2265121395507,-15.706069431219,-16.0002633602564,-15.4142526180666,-15.7102035458026,-16.4510368791384,-16.8750420060934,-17.1060356584383,-17.9530640601346,-19.1187810197741,-20.4968881161339,-22.3915011532511,-23.781958916929,-24.9416297339904,-25.1784628231843,-25.6014344214937,-25.3461011695388,-24.988345228782,-24.4606771786495,-23.5741163062508,-22.7769039852835,-22.0574130184838,-21.3364751115805,-21.1633073869703,-20.8304594865783,-20.0723662248568,-19.4354541968591,-18.961994724201,-18.3313872209431,-17.4099447567469,-16.850495700755,-16.2162191708051,-16.1793738745803,-15.9743734676785,-15.7934542782249,-15.7800184058294,-15.2101823869466,-14.5943026668915,-14.0912325985307,-13.6638685034763,-13.7840678849877,-13.0891748999587,-12.4878679338105,-12.0405567358924,-12.4698328589409]}]],[[{"lng":[36.4295100000001,36.32322,36.7538900000001,36.8525300000001,37.1674700000001,37.9040000000001,38.4100899594732,38.99062299984,39.266110060388,39.8142936541402,41.1792749366977,41.7349516131324,42.2768306821449,42.5895764503753,43.0812260272002,42.7796423683448,42.3515600000001,42.00975,41.59856,41.1552,40.8966,40.0262500000001,39.3406100000001,39.0994,38.51295,37.9060700000001,37.5937700000001,36.4295100000001],"lat":[14.4221100000004,14.8224899999998,16.2918599999997,16.9565500000006,17.2631400000001,17.4275399999998,17.99830739997,16.8406261255522,15.9227234969672,15.4356472844008,14.4910796167526,13.9210368921417,13.3439920109544,13.0004212508619,12.6996385767068,12.4554157576958,12.5422299999999,12.8658199999999,13.45209,13.7733299999999,14.1186399999999,14.5195899999999,14.5315500000002,14.7406400000002,14.50547,14.9594300000003,14.2131000000002,14.4221100000004]}]],[[{"lng":[47.78942,44.9636,43.66087,42.76967,42.12861,41.855083092644,41.1718,40.76848,39.85494,39.5593842587659,38.89251,38.67114,38.43697,38.120915,36.8550932380081,36.1590786328556,35.8174476623535,35.8174476623535,35.298007118233,34.70702,34.25032,34.0751,33.5682900000001,32.9541800000001,33.2948000000001,33.8255000000001,33.9749800000001,33.9616200000001,34.2574500000001,34.7311500000001,34.8316300000001,35.2604900000001,35.86363,36.2702200000001,36.4295100000001,37.5937700000001,37.9060700000001,38.51295,39.0994,39.3406100000001,40.0262500000001,40.8966,41.1552,41.59856,42.00975,42.3515600000001,42.0000000000001,41.6617600000001,41.7395900000002,41.7555700000002,42.3141400000001,42.5549300000001,42.776851841001,42.5587599999999,42.9281200000001,43.2969900000001,43.67875,46.9483400000001,47.78942],"lat":[8.00300000000016,5.00162000000004,4.95754999999995,4.25259000000017,4.23412999999991,3.91891192048376,3.91909000000011,4.25701999999984,3.83879000000044,3.42206000000053,3.50073999999981,3.61606999999994,3.58850999999996,3.59860500000003,4.4478641276728,4.4478641276728,4.77696566346184,5.33823208279071,5.5060000000001,6.59422,6.82607000000012,7.22595000000006,7.71333999999999,7.78497000000021,8.35458000000017,8.37916000000014,8.68456000000007,9.58358000000019,10.6300899999998,10.9101699999998,11.3189599999999,12.0828600000006,12.5782799999996,13.5633300000002,14.4221100000004,14.2131000000002,14.9594300000003,14.50547,14.7406400000002,14.5315500000002,14.5195899999999,14.1186399999999,13.7733299999999,13.45209,12.8658199999999,12.5422299999999,12.1000000000003,11.6311999999999,11.3551100000002,11.05091,11.0342000000002,11.1051100000002,10.9268785669341,10.5725800000003,10.0219400000006,9.54048000000011,9.18357999999981,7.99687999999993,8.00300000000016]}]],[[{"lng":[42.3515600000001,42.7796423683448,43.0812260272002,43.3178524106647,43.2863814633989,42.7158736508965,43.1453048032421,42.776851841001,42.5549300000001,42.3141400000001,41.7555700000002,41.7395900000002,41.6617600000001,42.0000000000001,42.3515600000001],"lat":[12.5422299999999,12.4554157576958,12.6996385767068,12.3901484237111,11.9749282902458,11.7356405705188,11.4620396997489,10.9268785669341,11.1051100000002,11.0342000000002,11.05091,11.3551100000002,11.6311999999999,12.1000000000003,12.5422299999999]}]],[[{"lng":[33.9037111971045,31.8661700000001,30.7698600000001,30.4191048520192,29.821518588996,29.5794661801409,29.5878377621722,29.8195032081366,29.8757788429024,30.0861535987627,30.4685075212903,30.8526701189481,31.1741492042358,30.77334679538,30.8338598975938,30.8338524217154,31.2455600000001,31.88145,32.6864200000001,33.3900000000001,34.005,34.47913,34.59607,35.03599,34.6721,34.18,33.8935689696669,33.9037111971045],"lat":[-0.94999999999952,-1.02736000000001,-1.01455000000005,-1.1346591121508,-1.44332244222969,-1.34131316488571,-0.587405694179271,-0.205310153813445,0.597379868976472,1.06231273030602,1.58380544677967,1.84939647054376,2.20446523682122,2.33988332764186,3.50916596111063,3.50917160422244,3.78190000000014,3.55826999999995,3.79231999999979,3.78999999999963,4.24988494736234,3.55560000000022,3.05374000000034,1.90583999999997,1.17694,0.515000000000006,0.109813537861806,-0.94999999999952]}]],[[{"lng":[30.4191048520192,30.8161348813177,30.7583089535831,30.46967,30.4696736457612,29.9383590024079,29.6321761410786,29.0249263852168,29.1174788754516,29.2548348324833,29.2918868344366,29.5794661801409,29.821518588996,30.4191048520192],"lat":[-1.1346591121508,-1.69891407634556,-2.28725025798838,-2.41383,-2.41385475710109,-2.3484868302543,-2.91785776124591,-2.83925790773017,-2.29221119548857,-2.21510995850913,-1.62005584066834,-1.34131316488571,-1.44332244222969,-1.1346591121508]}]],[[{"lng":[30.8338524217154,29.9535001970695,29.715995314256,29.1590784034465,28.6966776872988,28.4289937680269,27.9799772478428,27.3742261085175,27.2134090512252,26.4659094581232,26.2134184099451,25.7966479835112,25.1241308936647,25.1149324887168,24.5673690121521,23.8869795808607,24.1940677211877,24.537415163602,24.7949257454127,25.069603699344,25.7906333284139,25.962307049621,26.4773282132425,26.7520061671738,27.1125209817089,27.8335506107788,27.9708895877444,28.9665971707458,29.0009319149872,29.5159530786086,29.6189573113328,29.9966394979886,30.8378407319034,31.3528618955249,31.8507156870255,32.4000715948883,32.3142347342848,32.0738915245948,32.6747495488196,32.7434190373025,33.2069380845618,33.0867664797167,33.2069380845618,33.7219592481831,33.8421308530282,33.8249634809075,33.9633927949712,33.9749800000001,33.8255000000001,33.2948000000001,32.9541800000001,33.5682900000001,34.0751,34.25032,34.70702,35.298007118233,34.6201962678539,34.005,33.3900000000001,32.6864200000001,31.88145,31.2455600000001,30.8338524217154],"lat":[3.50917160422244,4.17369904216787,4.60080475506008,4.38926727947331,4.4550772159971,4.28715464926459,4.4084133976378,5.23394440350009,5.55095347739434,5.94671743410217,6.54660329836178,6.97931590415836,7.50008515057971,7.82510407147925,8.2291879337856,8.61972971293243,8.72869647240424,8.91753756573175,9.81024091600904,10.2737599632678,10.4110989402338,10.1364209863024,9.55273033419781,9.46689347359502,9.63856719480163,9.60423245056017,9.39822398511191,9.39822398511191,9.60423245056005,9.79307354388752,10.0849188699402,10.2909273353888,9.70723668328428,9.81024091600827,10.5312705450785,11.0806264529416,11.681484477166,11.9733298032187,12.0248319195808,12.2480077571497,12.1793382686664,11.4411412674762,10.7201116384068,10.3252620796304,9.98191463721596,9.48406084571541,9.46428522942106,8.68456000000007,8.37916000000014,8.35458000000017,7.78497000000021,7.71333999999999,7.22595000000006,6.82607000000012,6.59422,5.5060000000001,4.84712274208211,4.24988494736234,3.78999999999963,3.79231999999979,3.55826999999995,3.78190000000014,3.50917160422244]}]]],["Tanzania","Somalia","Kenya","Zimbabwe","Zambia","Malawi","Mozambique","Burundi","Madagascar","Eritrea","Ethiopia","Djibouti","Uganda","Rwanda","South.Sudan"],"eastern_africa",{"interactive":true,"className":"","pane":"tmap401","stroke":true,"color":"#666666","weight":1,"opacity":1,"fill":true,"fillColor":["#CC4C02","#F88B22","#F88B22","#FECF66","#CC4C02","#F88B22","#F88B22","#F88B22","#F88B22","#BFBFBF","#F88B22","#FEF6BA","#CC4C02","#F88B22","#CC4C02"],"fillOpacity":[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],"dashArray":"none","smoothFactor":1,"noClip":false},["<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Tanzania<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>pop_growth<\/nobr><\/td><td align=\"right\"><nobr>3.108<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Somalia<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>pop_growth<\/nobr><\/td><td align=\"right\"><nobr>2.858<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Kenya<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>pop_growth<\/nobr><\/td><td align=\"right\"><nobr>2.636<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Zimbabwe<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>pop_growth<\/nobr><\/td><td align=\"right\"><nobr>2.345<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Zambia<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>pop_growth<\/nobr><\/td><td align=\"right\"><nobr>3.040<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Malawi<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>pop_growth<\/nobr><\/td><td align=\"right\"><nobr>2.923<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Mozambique<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>pop_growth<\/nobr><\/td><td align=\"right\"><nobr>2.901<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Burundi<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>pop_growth<\/nobr><\/td><td align=\"right\"><nobr>2.992<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Madagascar<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>pop_growth<\/nobr><\/td><td align=\"right\"><nobr>2.701<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Eritrea<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>pop_growth<\/nobr><\/td><td align=\"right\"><nobr>  NA<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Ethiopia<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>pop_growth<\/nobr><\/td><td align=\"right\"><nobr>2.579<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Djibouti<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>pop_growth<\/nobr><\/td><td align=\"right\"><nobr>1.711<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Uganda<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>pop_growth<\/nobr><\/td><td align=\"right\"><nobr>3.351<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>Rwanda<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>pop_growth<\/nobr><\/td><td align=\"right\"><nobr>2.501<\/nobr><\/td><\/tr><\/table><\/div>","<style> div.leaflet-popup-content {width:auto !important;overflow-y:auto; overflow-x:hidden;}<\/style><div style=\"max-height:25em;padding-right:0px;\"><table>\n\t\t\t   <thead><tr><th colspan=\"2\"><b>South Sudan<\/b><\/th><\/thead><\/tr><tr><td style=\"color: #888888;\"><nobr>pop_growth<\/nobr><\/td><td align=\"right\"><nobr>3.113<\/nobr><\/td><\/tr><\/table><\/div>"],null,["Tanzania","Somalia","Kenya","Zimbabwe","Zambia","Malawi","Mozambique","Burundi","Madagascar","Eritrea","Ethiopia","Djibouti","Uganda","Rwanda","South Sudan"],{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addLegend","args":[{"colors":["#FEF6BA","#FECF66","#F88B22","#CC4C02","#BFBFBF"],"labels":["1.5 to 2.0","2.0 to 2.5","2.5 to 3.0","3.0 to 3.5","Missing"],"na_color":null,"na_label":"NA","opacity":1,"position":"topright","type":"unknown","title":"pop_growth","extra":null,"layerId":"legend401","className":"info legend","group":"eastern_africa"}]},{"method":"addLayersControl","args":[["Esri.WorldGrayCanvas","OpenStreetMap","Esri.WorldTopoMap"],"eastern_africa",{"collapsed":true,"autoZIndex":true,"position":"topleft"}]},{"method":"addScaleBar","args":[{"maxWidth":100,"metric":true,"imperial":false,"updateWhenIdle":true,"position":"bottomright"}]}],"limits":{"lat":[-26.7421916643359,17.99830739997],"lng":[21.8878426449539,51.13387]},"fitBounds":[-26.7421916643359,21.8878426449539,17.99830739997,51.13387,[]]},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->

``` r
tmap_mode("plot")
```

    ## tmap mode set to plotting

``` r
mapview::mapview(eastern_africa, zcol = "pop_growth")
```

<!--html_preserve-->

<div id="htmlwidget-85b89a0214ab1711af4a" class="leaflet html-widget" style="width:672px;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-85b89a0214ab1711af4a">{"x":{"options":{"minZoom":1,"maxZoom":52,"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}},"preferCanvas":false,"bounceAtZoomLimits":false,"maxBounds":[[[-90,-370]],[[90,370]]]},"calls":[{"method":"addProviderTiles","args":["CartoDB.Positron","CartoDB.Positron","CartoDB.Positron",{"errorTileUrl":"","noWrap":false,"detectRetina":false,"pane":"tilePane"}]},{"method":"addProviderTiles","args":["CartoDB.DarkMatter","CartoDB.DarkMatter","CartoDB.DarkMatter",{"errorTileUrl":"","noWrap":false,"detectRetina":false,"pane":"tilePane"}]},{"method":"addProviderTiles","args":["OpenStreetMap","OpenStreetMap","OpenStreetMap",{"errorTileUrl":"","noWrap":false,"detectRetina":false,"pane":"tilePane"}]},{"method":"addProviderTiles","args":["Esri.WorldImagery","Esri.WorldImagery","Esri.WorldImagery",{"errorTileUrl":"","noWrap":false,"detectRetina":false,"pane":"tilePane"}]},{"method":"addProviderTiles","args":["OpenTopoMap","OpenTopoMap","OpenTopoMap",{"errorTileUrl":"","noWrap":false,"detectRetina":false,"pane":"tilePane"}]},{"method":"createMapPane","args":["polygon",420]},{"method":"addPolygons","args":[[[[{"lng":[33.9037111971045,34.07262,37.6986899999999,37.7669,39.20222,38.74054,38.7997700000001,39.44,39.4700000000001,39.19469,39.25203,39.1865200000001,39.5357400000001,39.9496,40.3165862291108,40.31659,39.521,38.4275565935878,37.82764,37.47129,36.7751509946228,36.5140816586843,35.312397902169,34.5599890479994,34.28,33.9408377240965,33.7397200000001,32.7593754412213,32.1918648617919,31.5563480974665,31.1577513369501,30.7400097314221,30.7400154965518,30.1999967791017,29.62003217949,29.4199927100882,29.5199866065729,29.3399975929003,29.7535124040999,30.11632,30.50554,30.7522400000001,30.74301,30.52766,30.4696736457612,30.46967,30.7583089535831,30.8161348813177,30.4191048520192,30.7698600000001,31.8661700000001,33.9037111971045],"lat":[-0.94999999999952,-1.05981999999978,-3.0969899999995,-3.67711999999945,-4.67677000000032,-5.90894999999988,-6.47566,-6.83999999999994,-7.09999999999996,-7.70390000000006,-8.00781000000007,-8.48550999999984,-9.11237000000013,-10.0984000000006,-10.3170977528174,-10.3170999999999,-10.8968800000001,-11.2852023250816,-11.2687899999998,-11.5687600000001,-11.5945374487813,-11.7209380021671,-11.4391464168795,-11.5200200334156,-10.1599999999998,-9.69367384198043,-9.41714999999955,-9.23059905358901,-8.93035898197308,-8.76204884199826,-8.59457874731718,-8.34000593035405,-8.34000741947069,-7.07998097089848,-6.52001515058319,-5.93999887453942,-5.41997893638628,-4.49998341229434,-4.45238941815333,-4.09012000000008,-3.56858000000006,-3.35931000000007,-3.03430999999938,-2.80761999999986,-2.41385475710109,-2.41383,-2.28725025798838,-1.69891407634556,-1.1346591121508,-1.01455000000005,-1.02736000000001,-0.94999999999952]}]],[[{"lng":[41.58513,40.993,40.98105,41.855083092644,42.12861,42.76967,43.66087,44.9636,47.78942,48.4867358742269,48.9381295102964,48.938232863161,48.9384912453225,48.9420052427183,48.9482047585097,48.9482047585099,49.26776,49.72862,50.25878,50.73202,51.1112,51.13387,51.04153,51.04531,50.83418,50.55239,50.07092,49.4527,48.59455,47.74079,46.56476,45.56399,44.06815,43.13597,42.04157,41.81095,41.58513],"lat":[-1.6832500000002,-0.858290000000163,2.78452000000011,3.91891192048376,4.23412999999991,4.25259000000017,4.95754999999995,5.00162000000004,8.00300000000016,8.83762624758973,9.45174896894627,9.97350006758193,10.9823273787832,11.3942660587987,11.410617281698,11.410617281698,11.43033,11.5789,11.67957,12.0219000000002,12.0246399999998,11.7481500000002,11.1665100000001,10.6409000000002,10.2797199999994,9.19873999999999,8.08173000000019,6.80466000000015,5.33910999999971,4.21940000000015,2.85528999999983,2.04575999999946,1.05282999999993,0.292200000000048,-0.919160000000231,-1.44647000000012,-1.6832500000002]}]],[[{"lng":[39.20222,37.7669,37.6986899999999,34.07262,33.9037111971045,33.8935689696669,34.18,34.6721,35.03599,34.59607,34.47913,34.005,34.6201962678539,35.298007118233,35.8174476623535,35.8174476623535,36.1590786328556,36.8550932380081,38.120915,38.43697,38.67114,38.89251,39.5593842587659,39.85494,40.76848,41.1718,41.855083092644,40.98105,40.993,41.58513,40.88477,40.63785,40.26304,40.12119,39.80006,39.60489,39.20222],"lat":[-4.67677000000032,-3.67711999999945,-3.0969899999995,-1.05981999999978,-0.94999999999952,0.109813537861806,0.515000000000006,1.17694,1.90583999999997,3.05374000000034,3.55560000000022,4.24988494736234,4.84712274208211,5.5060000000001,5.33823208279071,4.77696566346184,4.4478641276728,4.4478641276728,3.59860500000003,3.58850999999996,3.61606999999994,3.50073999999981,3.42206000000053,3.83879000000044,4.25701999999984,3.91909000000011,3.91891192048376,2.78452000000011,-0.858290000000163,-1.6832500000002,-2.08254999999989,-2.49979000000018,-2.57309000000051,-3.2776800000002,-3.68116000000003,-4.34653000000029,-4.67677000000032]}]],[[{"lng":[31.1914091326213,30.6598653500671,30.3228833350918,29.839036899543,29.432188348109,28.7946562029242,28.0213700701086,27.7272278175033,27.7247473487533,27.2965047543505,26.1647908871585,25.8503914730947,25.6491634457502,25.264225701608,26.3819352556489,26.7067733090356,27.0444271176307,27.5982434425028,28.4679061215427,28.8258687680285,28.9474634132113,29.5168343442031,30.2742558123051,30.3389547055345,31.1730639991577,31.6364982439512,31.8520406430406,32.3282389666102,32.8476387875758,32.8498608741644,32.6548856951271,32.6119942563249,32.7727079607526,32.6597432797626,32.5086930681734,32.244988234188,31.1914091326213],"lat":[-22.2515096981722,-22.1515674781197,-22.2716118303337,-22.1022164852806,-22.0913127580673,-21.6394540341074,-21.4859750302009,-20.8518018531154,-20.4990585262905,-20.3915198706908,-19.2930856258949,-18.7144129370907,-18.5360258928189,-17.7365398088312,-17.8460421688585,-17.9612289364363,-17.9380262183375,-17.2908305803141,-16.468400160389,-16.3897486304404,-16.0430514461939,-15.6446778296568,-15.5077869605151,-15.88083912523,-15.8609436987981,-16.0719902482778,-16.319417006091,-16.3920740698937,-16.7133981258845,-17.9790573055772,-18.6720899390439,-19.4193828264162,-19.7155921363131,-20.3042900529826,-20.3952922502481,-21.1164885393136,-22.2515096981722]}]],[[{"lng":[30.7400097314221,31.1577513369501,31.5563480974665,32.1918648617919,32.7593754412213,33.2313879737753,33.4856876970836,33.3153104998173,33.1142891782019,33.3064221534631,32.9917643572379,32.6881653175231,33.2140246925252,30.1794812354818,30.2742558123051,29.5168343442031,28.9474634132113,28.8258687680285,28.4679061215427,27.5982434425028,27.0444271176307,26.7067733090356,26.3819352556489,25.264225701608,25.0844433936646,25.0769503109823,24.6823490740015,24.0338615251708,23.2150484555061,22.5624784685243,21.8878426449539,21.9338863461259,24.0161365088947,23.9309220720454,24.0799052263428,23.9041536801182,24.0178935075926,23.9122152035557,24.257155389104,24.314516228948,24.783169793403,25.4181181169732,25.7523096046047,26.5530875993996,27.1644197934125,27.3887988624238,28.15510867688,28.523561639121,28.9342859229768,29.6996138852195,29.6160014177712,29.3415478858691,28.6424174333924,28.3722530453704,28.4960697771418,28.6736816749289,28.4498710466728,28.7348665707625,29.0029122250605,30.3460860531908,30.7400097314221],"lat":[-8.34000593035405,-8.59457874731718,-8.76204884199826,-8.93035898197308,-9.23059905358901,-9.67672169356442,-10.5255587703907,-10.7965499813296,-11.6071981746928,-12.4357780900607,-12.7838705379785,-13.7128577612893,-13.9718600399363,-14.7960991349914,-15.5077869605151,-15.6446778296568,-16.0430514461939,-16.3897486304404,-16.468400160389,-17.2908305803141,-17.9380262183375,-17.9612289364363,-17.8460421688585,-17.7365398088312,-17.6618156877379,-17.5788233374766,-17.3534107398196,-17.2958431942464,-17.5231161434662,-16.8984514299222,-16.0803101538769,-12.8984371883689,-12.9110462378484,-12.5658476701392,-12.1912968888866,-11.7222815894063,-11.2372982723473,-10.9268262671378,-10.9519926896635,-11.2628264298988,-11.2386935360188,-11.3309359676598,-11.7849651017761,-11.9244397925321,-11.6087484676612,-12.1327474911006,-12.2724805640176,-12.6986044246971,-13.2489584286049,-13.2572266577718,-12.1788945451371,-12.3607439103724,-11.9715686987823,-11.7936467424016,-10.7898837215639,-9.60592498132493,-9.16491830814593,-8.52655934004487,-8.40703175215326,-8.23825652428861,-8.34000593035405]}]],[[{"lng":[32.7593754412213,33.7397200000001,33.9408377240965,34.28,34.5599890479994,34.280006137842,34.5599890479994,34.9071513201362,35.267956170398,35.6868453305559,35.7719047381084,35.3390629412316,35.0338102556835,34.3812919451341,34.3072912940921,34.5176660499523,34.4596334164885,34.0648254737786,33.7897001482567,33.2140246925252,32.6881653175231,32.9917643572379,33.3064221534631,33.1142891782019,33.3153104998173,33.4856876970836,33.2313879737753,32.7593754412213],"lat":[-9.23059905358901,-9.41714999999955,-9.69367384198043,-10.1599999999998,-11.5200200334156,-12.2800253231325,-13.5799976538667,-13.5654248999604,-13.8878341610299,-14.6110458309544,-15.8968588192406,-16.1074402808302,-16.8012997372127,-16.1835596655961,-15.4786414527026,-15.0137085913727,-14.6130095353814,-14.3599500464486,-14.4518307430629,-13.9718600399363,-13.7128577612893,-12.7838705379785,-12.4357780900607,-11.6071981746928,-10.7965499813296,-10.5255587703907,-9.67672169356442,-9.23059905358901]}]],[[{"lng":[34.5599890479994,35.312397902169,36.5140816586843,36.7751509946228,37.47129,37.82764,38.4275565935878,39.521,40.31659,40.3165862291108,40.3165885760172,40.478387485523,40.4372530454187,40.5608113950286,40.5996203956798,40.775475294769,40.4772506040126,40.0892639503652,39.4525586280971,38.5383508644215,37.4111328468389,36.2812793312094,35.8964966163641,35.1983996925331,34.78638349787,34.7018925310728,35.1761271502154,35.3734277687057,35.3858482537054,35.5625455363691,35.5339347674043,35.3717741228724,35.6074703305556,35.4587455584196,35.0407348976107,34.2158240089355,33.013210076639,32.5746321957779,32.6603633969501,32.9159550310657,32.8301204770289,32.0716654802811,31.985779249812,31.8377779477281,31.7524084815819,31.9305888201242,31.6703979835347,31.1914091326213,32.244988234188,32.5086930681734,32.6597432797626,32.7727079607526,32.6119942563249,32.6548856951271,32.8498608741644,32.8476387875758,32.3282389666102,31.8520406430406,31.6364982439512,31.1730639991577,30.3389547055345,30.2742558123051,30.1794812354818,33.2140246925252,33.7897001482567,34.0648254737786,34.4596334164885,34.5176660499523,34.3072912940921,34.3812919451341,35.0338102556835,35.3390629412316,35.7719047381084,35.6868453305559,35.267956170398,34.9071513201362,34.5599890479994,34.280006137842,34.5599890479994],"lat":[-11.5200200334156,-11.4391464168795,-11.7209380021671,-11.5945374487813,-11.5687600000001,-11.2687899999998,-11.2852023250816,-10.8968800000001,-10.3170999999999,-10.3170977528174,-10.3170960425258,-10.7654407690899,-11.7617107072449,-12.6391765275609,-14.2019751929318,-14.6917644181943,-15.406294447494,-16.1007740210645,-16.7208912085672,-17.1010230445064,-17.586368096591,-18.6596875952932,-18.8422604305813,-19.5528113745937,-19.7840117326676,-20.497043145431,-21.2543612606687,-21.8408370907486,-22.14,-22.0900000000002,-23.0707878557272,-23.5353589820318,-23.7065630022146,-24.1226099585968,-24.4783505184933,-24.8163143856823,-25.3575733375077,-25.7273182105557,-26.1485844865995,-26.2158672014436,-26.7421916643359,-26.7338200823048,-26.29177988048,-25.8433318010512,-25.4842839494874,-24.3694165992224,-23.6589690080741,-22.2515096981722,-21.1164885393136,-20.3952922502481,-20.3042900529826,-19.7155921363131,-19.4193828264162,-18.6720899390439,-17.9790573055772,-16.7133981258845,-16.3920740698937,-16.319417006091,-16.0719902482778,-15.8609436987981,-15.88083912523,-15.5077869605151,-14.7960991349914,-13.9718600399363,-14.4518307430629,-14.3599500464486,-14.6130095353814,-15.0137085913727,-15.4786414527026,-16.1835596655961,-16.8012997372127,-16.1074402808302,-15.8968588192406,-14.6110458309544,-13.8878341610299,-13.5654248999604,-13.5799976538667,-12.2800253231325,-11.5200200334156]}]],[[{"lng":[30.4696736457612,30.52766,30.74301,30.7522400000001,30.50554,30.11632,29.7535124040999,29.3399975929003,29.2763839047491,29.0249263852168,29.6321761410786,29.9383590024079,30.4696736457612],"lat":[-2.41385475710109,-2.80761999999986,-3.03430999999938,-3.35931000000007,-3.56858000000006,-4.09012000000008,-4.45238941815333,-4.49998341229434,-3.29390715903389,-2.83925790773017,-2.91785776124591,-2.3484868302543,-2.41385475710109]}]],[[{"lng":[49.5435189145958,49.8089807472791,50.0565108579572,50.2174312681141,50.4765368996255,50.377111443896,50.2002746925932,49.8606055031387,49.6726066424609,49.8633443540502,49.7745642433727,49.4986120949341,49.4356185239703,49.0417924334739,48.548540887248,47.9307491391987,47.5477234230513,47.0957613462266,46.2824776548171,45.4095076841105,44.8335738462176,44.0397204933498,43.7637683449112,43.6977775408745,43.3456543312376,43.254187046081,43.4332975604046,43.8936828956929,43.8963700701721,44.3743253924397,44.4643974139244,44.2324219093662,44.0429761085842,43.9630843442609,44.3124687029863,44.4465173683514,44.9449365578065,45.502731967965,45.8729936053363,46.3122432798172,46.8821826515643,47.7051298358124,48.0052148781313,47.8690474790422,48.2938277524814,48.8450602557388,48.863508742067,49.1946513201933,49.5435189145958],"lat":[-12.4698328589409,-12.8952849259995,-13.5557614071217,-14.7587887508766,-15.2265121395507,-15.706069431219,-16.0002633602564,-15.4142526180666,-15.7102035458026,-16.4510368791384,-16.8750420060934,-17.1060356584383,-17.9530640601346,-19.1187810197741,-20.4968881161339,-22.3915011532511,-23.781958916929,-24.9416297339904,-25.1784628231843,-25.6014344214937,-25.3461011695388,-24.988345228782,-24.4606771786495,-23.5741163062508,-22.7769039852835,-22.0574130184838,-21.3364751115805,-21.1633073869703,-20.8304594865783,-20.0723662248568,-19.4354541968591,-18.961994724201,-18.3313872209431,-17.4099447567469,-16.850495700755,-16.2162191708051,-16.1793738745803,-15.9743734676785,-15.7934542782249,-15.7800184058294,-15.2101823869466,-14.5943026668915,-14.0912325985307,-13.6638685034763,-13.7840678849877,-13.0891748999587,-12.4878679338105,-12.0405567358924,-12.4698328589409]}]],[[{"lng":[36.4295100000001,36.32322,36.7538900000001,36.8525300000001,37.1674700000001,37.9040000000001,38.4100899594732,38.99062299984,39.266110060388,39.8142936541402,41.1792749366977,41.7349516131324,42.2768306821449,42.5895764503753,43.0812260272002,42.7796423683448,42.3515600000001,42.00975,41.59856,41.1552,40.8966,40.0262500000001,39.3406100000001,39.0994,38.51295,37.9060700000001,37.5937700000001,36.4295100000001],"lat":[14.4221100000004,14.8224899999998,16.2918599999997,16.9565500000006,17.2631400000001,17.4275399999998,17.99830739997,16.8406261255522,15.9227234969672,15.4356472844008,14.4910796167526,13.9210368921417,13.3439920109544,13.0004212508619,12.6996385767068,12.4554157576958,12.5422299999999,12.8658199999999,13.45209,13.7733299999999,14.1186399999999,14.5195899999999,14.5315500000002,14.7406400000002,14.50547,14.9594300000003,14.2131000000002,14.4221100000004]}]],[[{"lng":[47.78942,44.9636,43.66087,42.76967,42.12861,41.855083092644,41.1718,40.76848,39.85494,39.5593842587659,38.89251,38.67114,38.43697,38.120915,36.8550932380081,36.1590786328556,35.8174476623535,35.8174476623535,35.298007118233,34.70702,34.25032,34.0751,33.5682900000001,32.9541800000001,33.2948000000001,33.8255000000001,33.9749800000001,33.9616200000001,34.2574500000001,34.7311500000001,34.8316300000001,35.2604900000001,35.86363,36.2702200000001,36.4295100000001,37.5937700000001,37.9060700000001,38.51295,39.0994,39.3406100000001,40.0262500000001,40.8966,41.1552,41.59856,42.00975,42.3515600000001,42.0000000000001,41.6617600000001,41.7395900000002,41.7555700000002,42.3141400000001,42.5549300000001,42.776851841001,42.5587599999999,42.9281200000001,43.2969900000001,43.67875,46.9483400000001,47.78942],"lat":[8.00300000000016,5.00162000000004,4.95754999999995,4.25259000000017,4.23412999999991,3.91891192048376,3.91909000000011,4.25701999999984,3.83879000000044,3.42206000000053,3.50073999999981,3.61606999999994,3.58850999999996,3.59860500000003,4.4478641276728,4.4478641276728,4.77696566346184,5.33823208279071,5.5060000000001,6.59422,6.82607000000012,7.22595000000006,7.71333999999999,7.78497000000021,8.35458000000017,8.37916000000014,8.68456000000007,9.58358000000019,10.6300899999998,10.9101699999998,11.3189599999999,12.0828600000006,12.5782799999996,13.5633300000002,14.4221100000004,14.2131000000002,14.9594300000003,14.50547,14.7406400000002,14.5315500000002,14.5195899999999,14.1186399999999,13.7733299999999,13.45209,12.8658199999999,12.5422299999999,12.1000000000003,11.6311999999999,11.3551100000002,11.05091,11.0342000000002,11.1051100000002,10.9268785669341,10.5725800000003,10.0219400000006,9.54048000000011,9.18357999999981,7.99687999999993,8.00300000000016]}]],[[{"lng":[42.3515600000001,42.7796423683448,43.0812260272002,43.3178524106647,43.2863814633989,42.7158736508965,43.1453048032421,42.776851841001,42.5549300000001,42.3141400000001,41.7555700000002,41.7395900000002,41.6617600000001,42.0000000000001,42.3515600000001],"lat":[12.5422299999999,12.4554157576958,12.6996385767068,12.3901484237111,11.9749282902458,11.7356405705188,11.4620396997489,10.9268785669341,11.1051100000002,11.0342000000002,11.05091,11.3551100000002,11.6311999999999,12.1000000000003,12.5422299999999]}]],[[{"lng":[33.9037111971045,31.8661700000001,30.7698600000001,30.4191048520192,29.821518588996,29.5794661801409,29.5878377621722,29.8195032081366,29.8757788429024,30.0861535987627,30.4685075212903,30.8526701189481,31.1741492042358,30.77334679538,30.8338598975938,30.8338524217154,31.2455600000001,31.88145,32.6864200000001,33.3900000000001,34.005,34.47913,34.59607,35.03599,34.6721,34.18,33.8935689696669,33.9037111971045],"lat":[-0.94999999999952,-1.02736000000001,-1.01455000000005,-1.1346591121508,-1.44332244222969,-1.34131316488571,-0.587405694179271,-0.205310153813445,0.597379868976472,1.06231273030602,1.58380544677967,1.84939647054376,2.20446523682122,2.33988332764186,3.50916596111063,3.50917160422244,3.78190000000014,3.55826999999995,3.79231999999979,3.78999999999963,4.24988494736234,3.55560000000022,3.05374000000034,1.90583999999997,1.17694,0.515000000000006,0.109813537861806,-0.94999999999952]}]],[[{"lng":[30.4191048520192,30.8161348813177,30.7583089535831,30.46967,30.4696736457612,29.9383590024079,29.6321761410786,29.0249263852168,29.1174788754516,29.2548348324833,29.2918868344366,29.5794661801409,29.821518588996,30.4191048520192],"lat":[-1.1346591121508,-1.69891407634556,-2.28725025798838,-2.41383,-2.41385475710109,-2.3484868302543,-2.91785776124591,-2.83925790773017,-2.29221119548857,-2.21510995850913,-1.62005584066834,-1.34131316488571,-1.44332244222969,-1.1346591121508]}]],[[{"lng":[30.8338524217154,29.9535001970695,29.715995314256,29.1590784034465,28.6966776872988,28.4289937680269,27.9799772478428,27.3742261085175,27.2134090512252,26.4659094581232,26.2134184099451,25.7966479835112,25.1241308936647,25.1149324887168,24.5673690121521,23.8869795808607,24.1940677211877,24.537415163602,24.7949257454127,25.069603699344,25.7906333284139,25.962307049621,26.4773282132425,26.7520061671738,27.1125209817089,27.8335506107788,27.9708895877444,28.9665971707458,29.0009319149872,29.5159530786086,29.6189573113328,29.9966394979886,30.8378407319034,31.3528618955249,31.8507156870255,32.4000715948883,32.3142347342848,32.0738915245948,32.6747495488196,32.7434190373025,33.2069380845618,33.0867664797167,33.2069380845618,33.7219592481831,33.8421308530282,33.8249634809075,33.9633927949712,33.9749800000001,33.8255000000001,33.2948000000001,32.9541800000001,33.5682900000001,34.0751,34.25032,34.70702,35.298007118233,34.6201962678539,34.005,33.3900000000001,32.6864200000001,31.88145,31.2455600000001,30.8338524217154],"lat":[3.50917160422244,4.17369904216787,4.60080475506008,4.38926727947331,4.4550772159971,4.28715464926459,4.4084133976378,5.23394440350009,5.55095347739434,5.94671743410217,6.54660329836178,6.97931590415836,7.50008515057971,7.82510407147925,8.2291879337856,8.61972971293243,8.72869647240424,8.91753756573175,9.81024091600904,10.2737599632678,10.4110989402338,10.1364209863024,9.55273033419781,9.46689347359502,9.63856719480163,9.60423245056017,9.39822398511191,9.39822398511191,9.60423245056005,9.79307354388752,10.0849188699402,10.2909273353888,9.70723668328428,9.81024091600827,10.5312705450785,11.0806264529416,11.681484477166,11.9733298032187,12.0248319195808,12.2480077571497,12.1793382686664,11.4411412674762,10.7201116384068,10.3252620796304,9.98191463721596,9.48406084571541,9.46428522942106,8.68456000000007,8.37916000000014,8.35458000000017,7.78497000000021,7.71333999999999,7.22595000000006,6.82607000000012,6.59422,5.5060000000001,4.84712274208211,4.24988494736234,3.78999999999963,3.79231999999979,3.55826999999995,3.78190000000014,3.50917160422244]}]]],null,"eastern_africa - pop_growth",{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}},"pane":"polygon","stroke":true,"color":"#333333","weight":0.5,"opacity":[0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.6,0.9,0.9,0.9,0.9,0.9],"fill":true,"fillColor":["#A6DA42","#25C771","#00AC8E","#008A98","#73D25B","#25C771","#25C771","#73D25B","#00AC8E","#BEBEBE","#009B95","#4B0055","#FDE333","#009B95","#A6DA42"],"fillOpacity":[0.6,0.6,0.6,0.6,0.6,0.6,0.6,0.6,0.6,0.6,0.6,0.6,0.6,0.6,0.6],"smoothFactor":1,"noClip":false},["<html><head><link rel=\"stylesheet\" type=\"text/css\" href=\"lib/popup/popup.css\"><\/head><body><div class=\"scrollableContainer\"><table class=\"popup scrollable\" id=\"popup\"><tr class='coord'><td><\/td><td><b>Feature ID<\/b><\/td><td align='right'>1&emsp;<\/td><\/tr><tr class='alt'><td>1<\/td><td><b>name&emsp;<\/b><\/td><td align='right'>Tanzania&emsp;<\/td><\/tr><tr><td>2<\/td><td><b>subregion&emsp;<\/b><\/td><td align='right'>Eastern Africa&emsp;<\/td><\/tr><tr class='alt'><td>3<\/td><td><b>gdpPercap&emsp;<\/b><\/td><td align='right'>2402.0994&emsp;<\/td><\/tr><tr><td>4<\/td><td><b>HDI&emsp;<\/b><\/td><td align='right'>0.470&emsp;<\/td><\/tr><tr class='alt'><td>5<\/td><td><b>pop_growth&emsp;<\/b><\/td><td align='right'>3.107572&emsp;<\/td><\/tr><tr><td>6<\/td><td><b>geom&emsp;<\/b><\/td><td align='right'>sfc_MULTIPOLYGON&emsp;<\/td><\/tr><\/table><\/div><\/body><\/html>","<html><head><link rel=\"stylesheet\" type=\"text/css\" href=\"lib/popup/popup.css\"><\/head><body><div class=\"scrollableContainer\"><table class=\"popup scrollable\" id=\"popup\"><tr class='coord'><td><\/td><td><b>Feature ID<\/b><\/td><td align='right'>2&emsp;<\/td><\/tr><tr class='alt'><td>1<\/td><td><b>name&emsp;<\/b><\/td><td align='right'>Somalia&emsp;<\/td><\/tr><tr><td>2<\/td><td><b>subregion&emsp;<\/b><\/td><td align='right'>Eastern Africa&emsp;<\/td><\/tr><tr class='alt'><td>3<\/td><td><b>gdpPercap&emsp;<\/b><\/td><td align='right'>NA&emsp;<\/td><\/tr><tr><td>4<\/td><td><b>HDI&emsp;<\/b><\/td><td align='right'>NA&emsp;<\/td><\/tr><tr class='alt'><td>5<\/td><td><b>pop_growth&emsp;<\/b><\/td><td align='right'>2.858286&emsp;<\/td><\/tr><tr><td>6<\/td><td><b>geom&emsp;<\/b><\/td><td align='right'>sfc_MULTIPOLYGON&emsp;<\/td><\/tr><\/table><\/div><\/body><\/html>","<html><head><link rel=\"stylesheet\" type=\"text/css\" href=\"lib/popup/popup.css\"><\/head><body><div class=\"scrollableContainer\"><table class=\"popup scrollable\" id=\"popup\"><tr class='coord'><td><\/td><td><b>Feature ID<\/b><\/td><td align='right'>3&emsp;<\/td><\/tr><tr class='alt'><td>1<\/td><td><b>name&emsp;<\/b><\/td><td align='right'>Kenya&emsp;<\/td><\/tr><tr><td>2<\/td><td><b>subregion&emsp;<\/b><\/td><td align='right'>Eastern Africa&emsp;<\/td><\/tr><tr class='alt'><td>3<\/td><td><b>gdpPercap&emsp;<\/b><\/td><td align='right'>2753.2361&emsp;<\/td><\/tr><tr><td>4<\/td><td><b>HDI&emsp;<\/b><\/td><td align='right'>0.515&emsp;<\/td><\/tr><tr class='alt'><td>5<\/td><td><b>pop_growth&emsp;<\/b><\/td><td align='right'>2.636116&emsp;<\/td><\/tr><tr><td>6<\/td><td><b>geom&emsp;<\/b><\/td><td align='right'>sfc_MULTIPOLYGON&emsp;<\/td><\/tr><\/table><\/div><\/body><\/html>","<html><head><link rel=\"stylesheet\" type=\"text/css\" href=\"lib/popup/popup.css\"><\/head><body><div class=\"scrollableContainer\"><table class=\"popup scrollable\" id=\"popup\"><tr class='coord'><td><\/td><td><b>Feature ID<\/b><\/td><td align='right'>4&emsp;<\/td><\/tr><tr class='alt'><td>1<\/td><td><b>name&emsp;<\/b><\/td><td align='right'>Zimbabwe&emsp;<\/td><\/tr><tr><td>2<\/td><td><b>subregion&emsp;<\/b><\/td><td align='right'>Eastern Africa&emsp;<\/td><\/tr><tr class='alt'><td>3<\/td><td><b>gdpPercap&emsp;<\/b><\/td><td align='right'>1925.1387&emsp;<\/td><\/tr><tr><td>4<\/td><td><b>HDI&emsp;<\/b><\/td><td align='right'>0.387&emsp;<\/td><\/tr><tr class='alt'><td>5<\/td><td><b>pop_growth&emsp;<\/b><\/td><td align='right'>2.344799&emsp;<\/td><\/tr><tr><td>6<\/td><td><b>geom&emsp;<\/b><\/td><td align='right'>sfc_MULTIPOLYGON&emsp;<\/td><\/tr><\/table><\/div><\/body><\/html>","<html><head><link rel=\"stylesheet\" type=\"text/css\" href=\"lib/popup/popup.css\"><\/head><body><div class=\"scrollableContainer\"><table class=\"popup scrollable\" id=\"popup\"><tr class='coord'><td><\/td><td><b>Feature ID<\/b><\/td><td align='right'>5&emsp;<\/td><\/tr><tr class='alt'><td>1<\/td><td><b>name&emsp;<\/b><\/td><td align='right'>Zambia&emsp;<\/td><\/tr><tr><td>2<\/td><td><b>subregion&emsp;<\/b><\/td><td align='right'>Eastern Africa&emsp;<\/td><\/tr><tr class='alt'><td>3<\/td><td><b>gdpPercap&emsp;<\/b><\/td><td align='right'>3632.5038&emsp;<\/td><\/tr><tr><td>4<\/td><td><b>HDI&emsp;<\/b><\/td><td align='right'>0.443&emsp;<\/td><\/tr><tr class='alt'><td>5<\/td><td><b>pop_growth&emsp;<\/b><\/td><td align='right'>3.040211&emsp;<\/td><\/tr><tr><td>6<\/td><td><b>geom&emsp;<\/b><\/td><td align='right'>sfc_MULTIPOLYGON&emsp;<\/td><\/tr><\/table><\/div><\/body><\/html>","<html><head><link rel=\"stylesheet\" type=\"text/css\" href=\"lib/popup/popup.css\"><\/head><body><div class=\"scrollableContainer\"><table class=\"popup scrollable\" id=\"popup\"><tr class='coord'><td><\/td><td><b>Feature ID<\/b><\/td><td align='right'>6&emsp;<\/td><\/tr><tr class='alt'><td>1<\/td><td><b>name&emsp;<\/b><\/td><td align='right'>Malawi&emsp;<\/td><\/tr><tr><td>2<\/td><td><b>subregion&emsp;<\/b><\/td><td align='right'>Eastern Africa&emsp;<\/td><\/tr><tr class='alt'><td>3<\/td><td><b>gdpPercap&emsp;<\/b><\/td><td align='right'>1090.3672&emsp;<\/td><\/tr><tr><td>4<\/td><td><b>HDI&emsp;<\/b><\/td><td align='right'>0.415&emsp;<\/td><\/tr><tr class='alt'><td>5<\/td><td><b>pop_growth&emsp;<\/b><\/td><td align='right'>2.922940&emsp;<\/td><\/tr><tr><td>6<\/td><td><b>geom&emsp;<\/b><\/td><td align='right'>sfc_MULTIPOLYGON&emsp;<\/td><\/tr><\/table><\/div><\/body><\/html>","<html><head><link rel=\"stylesheet\" type=\"text/css\" href=\"lib/popup/popup.css\"><\/head><body><div class=\"scrollableContainer\"><table class=\"popup scrollable\" id=\"popup\"><tr class='coord'><td><\/td><td><b>Feature ID<\/b><\/td><td align='right'>7&emsp;<\/td><\/tr><tr class='alt'><td>1<\/td><td><b>name&emsp;<\/b><\/td><td align='right'>Mozambique&emsp;<\/td><\/tr><tr><td>2<\/td><td><b>subregion&emsp;<\/b><\/td><td align='right'>Eastern Africa&emsp;<\/td><\/tr><tr class='alt'><td>3<\/td><td><b>gdpPercap&emsp;<\/b><\/td><td align='right'>1079.8239&emsp;<\/td><\/tr><tr><td>4<\/td><td><b>HDI&emsp;<\/b><\/td><td align='right'>0.322&emsp;<\/td><\/tr><tr class='alt'><td>5<\/td><td><b>pop_growth&emsp;<\/b><\/td><td align='right'>2.900696&emsp;<\/td><\/tr><tr><td>6<\/td><td><b>geom&emsp;<\/b><\/td><td align='right'>sfc_MULTIPOLYGON&emsp;<\/td><\/tr><\/table><\/div><\/body><\/html>","<html><head><link rel=\"stylesheet\" type=\"text/css\" href=\"lib/popup/popup.css\"><\/head><body><div class=\"scrollableContainer\"><table class=\"popup scrollable\" id=\"popup\"><tr class='coord'><td><\/td><td><b>Feature ID<\/b><\/td><td align='right'>8&emsp;<\/td><\/tr><tr class='alt'><td>1<\/td><td><b>name&emsp;<\/b><\/td><td align='right'>Burundi&emsp;<\/td><\/tr><tr><td>2<\/td><td><b>subregion&emsp;<\/b><\/td><td align='right'>Eastern Africa&emsp;<\/td><\/tr><tr class='alt'><td>3<\/td><td><b>gdpPercap&emsp;<\/b><\/td><td align='right'> 803.1728&emsp;<\/td><\/tr><tr><td>4<\/td><td><b>HDI&emsp;<\/b><\/td><td align='right'>0.352&emsp;<\/td><\/tr><tr class='alt'><td>5<\/td><td><b>pop_growth&emsp;<\/b><\/td><td align='right'>2.992265&emsp;<\/td><\/tr><tr><td>6<\/td><td><b>geom&emsp;<\/b><\/td><td align='right'>sfc_MULTIPOLYGON&emsp;<\/td><\/tr><\/table><\/div><\/body><\/html>","<html><head><link rel=\"stylesheet\" type=\"text/css\" href=\"lib/popup/popup.css\"><\/head><body><div class=\"scrollableContainer\"><table class=\"popup scrollable\" id=\"popup\"><tr class='coord'><td><\/td><td><b>Feature ID<\/b><\/td><td align='right'>9&emsp;<\/td><\/tr><tr class='alt'><td>1<\/td><td><b>name&emsp;<\/b><\/td><td align='right'>Madagascar&emsp;<\/td><\/tr><tr><td>2<\/td><td><b>subregion&emsp;<\/b><\/td><td align='right'>Eastern Africa&emsp;<\/td><\/tr><tr class='alt'><td>3<\/td><td><b>gdpPercap&emsp;<\/b><\/td><td align='right'>1372.0210&emsp;<\/td><\/tr><tr><td>4<\/td><td><b>HDI&emsp;<\/b><\/td><td align='right'>0.483&emsp;<\/td><\/tr><tr class='alt'><td>5<\/td><td><b>pop_growth&emsp;<\/b><\/td><td align='right'>2.701097&emsp;<\/td><\/tr><tr><td>6<\/td><td><b>geom&emsp;<\/b><\/td><td align='right'>sfc_MULTIPOLYGON&emsp;<\/td><\/tr><\/table><\/div><\/body><\/html>","<html><head><link rel=\"stylesheet\" type=\"text/css\" href=\"lib/popup/popup.css\"><\/head><body><div class=\"scrollableContainer\"><table class=\"popup scrollable\" id=\"popup\"><tr class='coord'><td><\/td><td><b>Feature ID<\/b><\/td><td align='right'>10&emsp;<\/td><\/tr><tr class='alt'><td>1<\/td><td><b>name&emsp;<\/b><\/td><td align='right'>Eritrea&emsp;<\/td><\/tr><tr><td>2<\/td><td><b>subregion&emsp;<\/b><\/td><td align='right'>Eastern Africa&emsp;<\/td><\/tr><tr class='alt'><td>3<\/td><td><b>gdpPercap&emsp;<\/b><\/td><td align='right'>NA&emsp;<\/td><\/tr><tr><td>4<\/td><td><b>HDI&emsp;<\/b><\/td><td align='right'>0.346&emsp;<\/td><\/tr><tr class='alt'><td>5<\/td><td><b>pop_growth&emsp;<\/b><\/td><td align='right'>NA&emsp;<\/td><\/tr><tr><td>6<\/td><td><b>geom&emsp;<\/b><\/td><td align='right'>sfc_MULTIPOLYGON&emsp;<\/td><\/tr><\/table><\/div><\/body><\/html>","<html><head><link rel=\"stylesheet\" type=\"text/css\" href=\"lib/popup/popup.css\"><\/head><body><div class=\"scrollableContainer\"><table class=\"popup scrollable\" id=\"popup\"><tr class='coord'><td><\/td><td><b>Feature ID<\/b><\/td><td align='right'>11&emsp;<\/td><\/tr><tr class='alt'><td>1<\/td><td><b>name&emsp;<\/b><\/td><td align='right'>Ethiopia&emsp;<\/td><\/tr><tr><td>2<\/td><td><b>subregion&emsp;<\/b><\/td><td align='right'>Eastern Africa&emsp;<\/td><\/tr><tr class='alt'><td>3<\/td><td><b>gdpPercap&emsp;<\/b><\/td><td align='right'>1424.5270&emsp;<\/td><\/tr><tr><td>4<\/td><td><b>HDI&emsp;<\/b><\/td><td align='right'>0.392&emsp;<\/td><\/tr><tr class='alt'><td>5<\/td><td><b>pop_growth&emsp;<\/b><\/td><td align='right'>2.579068&emsp;<\/td><\/tr><tr><td>6<\/td><td><b>geom&emsp;<\/b><\/td><td align='right'>sfc_MULTIPOLYGON&emsp;<\/td><\/tr><\/table><\/div><\/body><\/html>","<html><head><link rel=\"stylesheet\" type=\"text/css\" href=\"lib/popup/popup.css\"><\/head><body><div class=\"scrollableContainer\"><table class=\"popup scrollable\" id=\"popup\"><tr class='coord'><td><\/td><td><b>Feature ID<\/b><\/td><td align='right'>12&emsp;<\/td><\/tr><tr class='alt'><td>1<\/td><td><b>name&emsp;<\/b><\/td><td align='right'>Djibouti&emsp;<\/td><\/tr><tr><td>2<\/td><td><b>subregion&emsp;<\/b><\/td><td align='right'>Eastern Africa&emsp;<\/td><\/tr><tr class='alt'><td>3<\/td><td><b>gdpPercap&emsp;<\/b><\/td><td align='right'>NA&emsp;<\/td><\/tr><tr><td>4<\/td><td><b>HDI&emsp;<\/b><\/td><td align='right'>0.442&emsp;<\/td><\/tr><tr class='alt'><td>5<\/td><td><b>pop_growth&emsp;<\/b><\/td><td align='right'>1.711182&emsp;<\/td><\/tr><tr><td>6<\/td><td><b>geom&emsp;<\/b><\/td><td align='right'>sfc_MULTIPOLYGON&emsp;<\/td><\/tr><\/table><\/div><\/body><\/html>","<html><head><link rel=\"stylesheet\" type=\"text/css\" href=\"lib/popup/popup.css\"><\/head><body><div class=\"scrollableContainer\"><table class=\"popup scrollable\" id=\"popup\"><tr class='coord'><td><\/td><td><b>Feature ID<\/b><\/td><td align='right'>13&emsp;<\/td><\/tr><tr class='alt'><td>1<\/td><td><b>name&emsp;<\/b><\/td><td align='right'>Uganda&emsp;<\/td><\/tr><tr><td>2<\/td><td><b>subregion&emsp;<\/b><\/td><td align='right'>Eastern Africa&emsp;<\/td><\/tr><tr class='alt'><td>3<\/td><td><b>gdpPercap&emsp;<\/b><\/td><td align='right'>1637.2751&emsp;<\/td><\/tr><tr><td>4<\/td><td><b>HDI&emsp;<\/b><\/td><td align='right'>0.454&emsp;<\/td><\/tr><tr class='alt'><td>5<\/td><td><b>pop_growth&emsp;<\/b><\/td><td align='right'>3.350650&emsp;<\/td><\/tr><tr><td>6<\/td><td><b>geom&emsp;<\/b><\/td><td align='right'>sfc_MULTIPOLYGON&emsp;<\/td><\/tr><\/table><\/div><\/body><\/html>","<html><head><link rel=\"stylesheet\" type=\"text/css\" href=\"lib/popup/popup.css\"><\/head><body><div class=\"scrollableContainer\"><table class=\"popup scrollable\" id=\"popup\"><tr class='coord'><td><\/td><td><b>Feature ID<\/b><\/td><td align='right'>14&emsp;<\/td><\/tr><tr class='alt'><td>1<\/td><td><b>name&emsp;<\/b><\/td><td align='right'>Rwanda&emsp;<\/td><\/tr><tr><td>2<\/td><td><b>subregion&emsp;<\/b><\/td><td align='right'>Eastern Africa&emsp;<\/td><\/tr><tr class='alt'><td>3<\/td><td><b>gdpPercap&emsp;<\/b><\/td><td align='right'>1629.8689&emsp;<\/td><\/tr><tr><td>4<\/td><td><b>HDI&emsp;<\/b><\/td><td align='right'>0.429&emsp;<\/td><\/tr><tr class='alt'><td>5<\/td><td><b>pop_growth&emsp;<\/b><\/td><td align='right'>2.500797&emsp;<\/td><\/tr><tr><td>6<\/td><td><b>geom&emsp;<\/b><\/td><td align='right'>sfc_MULTIPOLYGON&emsp;<\/td><\/tr><\/table><\/div><\/body><\/html>","<html><head><link rel=\"stylesheet\" type=\"text/css\" href=\"lib/popup/popup.css\"><\/head><body><div class=\"scrollableContainer\"><table class=\"popup scrollable\" id=\"popup\"><tr class='coord'><td><\/td><td><b>Feature ID<\/b><\/td><td align='right'>15&emsp;<\/td><\/tr><tr class='alt'><td>1<\/td><td><b>name&emsp;<\/b><\/td><td align='right'>South Sudan&emsp;<\/td><\/tr><tr><td>2<\/td><td><b>subregion&emsp;<\/b><\/td><td align='right'>Eastern Africa&emsp;<\/td><\/tr><tr class='alt'><td>3<\/td><td><b>gdpPercap&emsp;<\/b><\/td><td align='right'>1935.8794&emsp;<\/td><\/tr><tr><td>4<\/td><td><b>HDI&emsp;<\/b><\/td><td align='right'>NA&emsp;<\/td><\/tr><tr class='alt'><td>5<\/td><td><b>pop_growth&emsp;<\/b><\/td><td align='right'>3.113461&emsp;<\/td><\/tr><tr><td>6<\/td><td><b>geom&emsp;<\/b><\/td><td align='right'>sfc_MULTIPOLYGON&emsp;<\/td><\/tr><\/table><\/div><\/body><\/html>"],{"maxWidth":800,"minWidth":50,"autoPan":true,"keepInView":false,"closeButton":true,"closeOnClick":true,"className":""},["3.10757241505603","2.85828597763128","2.6361163551476","2.34479907008461","3.04021077215453","2.92294017355582","2.90069572870848","2.99226468715451","2.70109745083855",null,"2.57906830567915","1.7111823002921","3.35065035255058","2.5007965300158","3.11346113549839"],{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},{"stroke":true,"weight":1,"opacity":0.9,"fillOpacity":0.84,"bringToFront":false,"sendToBack":false}]},{"method":"addScaleBar","args":[{"maxWidth":100,"metric":true,"imperial":true,"updateWhenIdle":true,"position":"bottomleft"}]},{"method":"addHomeButton","args":[21.8878426449539,-26.7421916643359,51.13387,17.99830739997,"eastern_africa - pop_growth","Zoom to eastern_africa - pop_growth","<strong> eastern_africa - pop_growth <\/strong>","bottomright"]},{"method":"addLayersControl","args":[["CartoDB.Positron","CartoDB.DarkMatter","OpenStreetMap","Esri.WorldImagery","OpenTopoMap"],"eastern_africa - pop_growth",{"collapsed":true,"autoZIndex":true,"position":"topleft"}]},{"method":"addLegend","args":[{"colors":["#4B0055 , #481863 5.41747059880475%, #32427D 17.6165494234568%, #016691 29.8156282481089%, #018898 42.014707072761%, #02A591 54.2137858974131%, #12BE7D 66.4128647220651%, #73D25B 78.6119435467172%, #C7DE34 90.8110223713692%, #FDE333 "],"labels":["1.8","2.0","2.2","2.4","2.6","2.8","3.0","3.2"],"na_color":"#BEBEBE","na_label":"NA","opacity":1,"position":"topright","type":"numeric","title":"eastern_africa - pop_growth","extra":{"p_1":0.0541747059880475,"p_n":0.908110223713693},"layerId":null,"className":"info legend","group":"eastern_africa - pop_growth"}]}],"limits":{"lat":[-26.7421916643359,17.99830739997],"lng":[21.8878426449539,51.13387]},"fitBounds":[-26.7421916643359,21.8878426449539,17.99830739997,51.13387,[]]},"evals":[],"jsHooks":{"render":[{"code":"function(el, x, data) {\n  return (\n      function(el, x, data) {\n      // get the leaflet map\n      var map = this; //HTMLWidgets.find('#' + el.id);\n      // we need a new div element because we have to handle\n      // the mouseover output separately\n      // debugger;\n      function addElement () {\n      // generate new div Element\n      var newDiv = $(document.createElement('div'));\n      // append at end of leaflet htmlwidget container\n      $(el).append(newDiv);\n      //provide ID and style\n      newDiv.addClass('lnlt');\n      newDiv.css({\n      'position': 'relative',\n      'bottomleft':  '0px',\n      'background-color': 'rgba(255, 255, 255, 0.7)',\n      'box-shadow': '0 0 2px #bbb',\n      'background-clip': 'padding-box',\n      'margin': '0',\n      'padding-left': '5px',\n      'color': '#333',\n      'font': '9px/1.5 \"Helvetica Neue\", Arial, Helvetica, sans-serif',\n      'z-index': '700',\n      });\n      return newDiv;\n      }\n\n\n      // check for already existing lnlt class to not duplicate\n      var lnlt = $(el).find('.lnlt');\n\n      if(!lnlt.length) {\n      lnlt = addElement();\n\n      // grab the special div we generated in the beginning\n      // and put the mousmove output there\n\n      map.on('mousemove', function (e) {\n      if (e.originalEvent.ctrlKey) {\n      if (document.querySelector('.lnlt') === null) lnlt = addElement();\n      lnlt.text(\n                           ' lon: ' + (e.latlng.lng).toFixed(5) +\n                           ' | lat: ' + (e.latlng.lat).toFixed(5) +\n                           ' | zoom: ' + map.getZoom() +\n                           ' | x: ' + L.CRS.EPSG3857.project(e.latlng).x.toFixed(0) +\n                           ' | y: ' + L.CRS.EPSG3857.project(e.latlng).y.toFixed(0) +\n                           ' | epsg: 3857 ' +\n                           ' | proj4: +proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs ');\n      } else {\n      if (document.querySelector('.lnlt') === null) lnlt = addElement();\n      lnlt.text(\n                      ' lon: ' + (e.latlng.lng).toFixed(5) +\n                      ' | lat: ' + (e.latlng.lat).toFixed(5) +\n                      ' | zoom: ' + map.getZoom() + ' ');\n      }\n      });\n\n      // remove the lnlt div when mouse leaves map\n      map.on('mouseout', function (e) {\n      var strip = document.querySelector('.lnlt');\n      if( strip !==null) strip.remove();\n      });\n\n      };\n\n      //$(el).keypress(67, function(e) {\n      map.on('preclick', function(e) {\n      if (e.originalEvent.ctrlKey) {\n      if (document.querySelector('.lnlt') === null) lnlt = addElement();\n      lnlt.text(\n                      ' lon: ' + (e.latlng.lng).toFixed(5) +\n                      ' | lat: ' + (e.latlng.lat).toFixed(5) +\n                      ' | zoom: ' + map.getZoom() + ' ');\n      var txt = document.querySelector('.lnlt').textContent;\n      console.log(txt);\n      //txt.innerText.focus();\n      //txt.select();\n      setClipboardText('\"' + txt + '\"');\n      }\n      });\n\n      }\n      ).call(this.getMap(), el, x, data);\n}","data":null},{"code":"function(el, x, data) {\n  return (function(el,x,data){\n           var map = this;\n\n           map.on('keypress', function(e) {\n               console.log(e.originalEvent.code);\n               var key = e.originalEvent.code;\n               if (key === 'KeyE') {\n                   var bb = this.getBounds();\n                   var txt = JSON.stringify(bb);\n                   console.log(txt);\n\n                   setClipboardText('\\'' + txt + '\\'');\n               }\n           })\n        }).call(this.getMap(), el, x, data);\n}","data":null}]}}</script>

<!--/html_preserve-->

%let pgm=utl-identify-the-outer-most-points-in-a-graph-object-ai-convex-hulls;

Identify the outer most points in a graph object ai convex hull (also works for polygons)

related to
https://communities.sas.com/t5/SAS-Forecasting-and-Econometrics/Identify-Zigzag-point-for-timeseries/m-p/845543#M4559

   CONTENTS

      1 create envelop of outermost points
      2 related repos

hi res graphic output
https://tinyurl.com/3492zs9d
https://github.com/rogerjdeangelis/utl-identify-the-outer-most-points-in-a-graph-object-ai-convex-hulls/blob/main/hul.png

github
https://tinyurl.com/2p8h8rp6
https://github.com/rogerjdeangelis/utl-identify-the-outer-most-points-in-a-graph-object-ai-convex-hulls

/*               _     _
 _ __  _ __ ___ | |__ | | ___ _ __ ___
| `_ \| `__/ _ \| `_ \| |/ _ \ `_ ` _ \
| |_) | | | (_) | |_) | |  __/ | | | | |
| .__/|_|  \___/|_.__/|_|\___|_| |_| |_|
|_|
*/


/**************************************************************************************************************************/
/*                INPUT                 |              PROCESS                       |             OUTPUT                 */
/*                =====                 |              =======                       |             ======                 */
/*                                      |                                            | https://tinyurl.com/3492zs9d       */
/*                                      |                                            |                                    */
/*                                      |                                            |                                    */
/* Obs      X        Y                  | 1 CREATE ENVELOP OF OUTERMOST POINTS       |  0.0       1.1  X    2.2     3.3   */
/*                                      | ====================================       |  ----------+---------+---------+-  */
/*   1    0.00    1.03176               |                                            |  |                              |  */
/*   2    0.01    1.21848               | %utlfkil("d:/png/hul.png")                 |  |Convex hulls                  |  */
/*   3    0.02    1.16561               |                                            |  |                              |  */
/*   4    0.03    1.13006               | %utl_rbeginx;                              |  |All (x,y) points between lines|  */
/*   5    0.04    1.13513               | parmcards4;                                |  |                              |  */
/* ...                                  | # Load required libraries                  | 3+     /*                       +3 */
/* 311    3.10    1.05822               | library(ggplot2)                           |  |    / * *                     |  */
/* 312    3.11    1.13153               | library(dplyr)                             | Y|   |** ****                   |Y */
/* 313    3.12    1.05568               | library(haven)                             |  |   \* *    *                  |  */
/* 314    3.13    1.04113               | source("c:/oto/fn_tosas9x.R")              | 2+   * **    **                 +2 */
/* 315    3.14    1.06199               | have<-read_sas("d:/sd1/have.sas7bdat")     |  |  /  *      **           */   |  */
/*                                      | x=have$X;                                  |  | * *         **         */    |  */
/* options validvarname=upcase;         | y<-have$Y;                                 |  |              *         *     |  */
/* libname sd1 "d:/sd1";                | library(ggplot2)                           | 1+               **       *\    +1 */
/* data sd1.have;                       | library(dplyr)                             |  |                 *    **/     |  */
/*    call streaminit(54321);           |                                            |  |                  ** * /      |  */
/*   do x=0 to constant("PI")  by .01;  | # Step 1: Generate noisy sine wave data    |  |                     *        |  */
/*      y=sin(2*x) +                    | set.seed(123) # For reproducibility        | 0+                              +0 */
/*          rand('normal',0,.05) + 1.1; | x <- seq(0, 2 * pi, length.out = 500)      |  ----------+---------+---------+-  */
/*      output;                         | y <- sin(x) + rnorm(length(x), sd = 0.2)   |  0.0       1.1   X   2.2     3.3   */
/*   end;                               |                                            |                                    */
/* run;quit;                            | # Combine x and y into a data frame        |                                    */
/*                                      | data <- data.frame(x = x, y = y)           |           Outermost points         */
/*  0.0       1.1   X   2.2       3.3   | # Load required libraries                  |        (connect to get envelope)   */
/*  -+---------+---------+---------+-   |                                            |                                    */
/* 3+                               +3  | upper_hull_indices<-chull(data$x data$y)   |                                    */
/*  | y=sin(2*x)+1.1+ normal(0,.05) |   | upper_hull <-                              |             X         Y            */
/*  |                               |   |  data[upper_hull_indices,] %>% arrange(x)  |                                    */
/*  |      ***                      |   |                                            |           0.00     1.03176         */
/* 2+    *******                    +2  | upper_hull                                 |           0.01     1.22848         */
/*  |  ***     ***                  |   |                                            |           0.11     1.53293         */
/* Y| ***        **                 |Y  | png("d:/png/hul.png");                     |           0.21     1.75619         */
/*  |***          ***            *  |   |                                            |           0.31     1.94892         */
/* 1+*             **            ** +1  | ggplot(data, aes(x, y)) +                  |           0.47     2.16443         */
/*  |               ***        ***  |   |  geom_point(alpha = 0.5) +                 |           0.53     2.18238         */
/*  |                 **      **    |   |  geom_point(                               |           1.59    -0.04062         */
/*  |                  ********     |   |    data = upper_hull                       |           2.56     2.16590         */
/* 0 +                   *****     0+   |   ,aes(x, y)                               |           2.64     2.14605         */
/*  |                               |   |   ,color = "red"                           |           2.81     2.09272         */
/*  -+---------+---------+---------+-   |   ,size = 5) +                             |           3.11     1.28931         */
/*  0.0       1.1   X   2.2       3.3   |  labs(                                     |           3.14     1.06995         */
/*                                      |    title="Convex Hull Outermost points",   |                                    */
/*                                      |       x = "X",                             |                                    */
/*                                      |       y = "Y") +                           |                                    */
/*                                      |  theme_minimal()                           |                                    */
/*                                      | dev.off()                                  |                                    */
/*                                      | fn_tosas9x(                                |                                    */
/*                                      |       inp    = upr                         |                                    */
/*                                      |      ,outlib ="d:/sd1/"                    |                                    */
/*                                      |      ,outdsn ="upr"                        |                                    */
/*                                      |      )                                     |                                    */
/*                                      | ;;;;                                       |                                    */
/*                                      | %utl_rendx;                                |                                    */
/*                                      |                                            |                                    */
/*                                      | proc print data=sd1.upr;                   |                                    */
/*                                      | run;quit;                                  |                                    */
/*                                      |                                            |                                    */
/*                                      |                                            |                                    */
/*                                      |                                            |                                    */
/*                                      |                                            |                                    */
/*                                      |                                            |                                    */
/*                                      |                                            |                                    */
/*                                      |                                            |                                    */
/*                                      |                                            |                                    */
/*                                      |                                            |                                    */
/*                                      |                                            |                                    */
/*                                      |                                            |                                    */
/*                                      |                                            |                                    */
/*                                      |                                            |                                    */
/*                                      |                                            |                                    */
/**************************************************************************************************************************/

/*__ _   _ ____  _   _ _____
|_ _| \ | |  _ \| | | |_   _|
 | ||  \| | |_) | | | | | |
 | || |\  |  __/| |_| | | |
|___|_| \_|_|    \___/  |_|

*/

options validvarname=upcase;
libname sd1 "d:/sd1";
data sd1.have;
   call streaminit(54321);
  do x=0 to constant("PI")  by .01;
     y=sin(2*x) +
          rand('normal',0,.05) + 1.1;
     output;
  end;
run;quit;

/**************************************************************************************************************************/
/*                                                                                                                        */
/* Obs      X        Y                                                                                                    */
/*                                                                                                                        */
/*   1    0.00    1.03176                                                                                                 */
/*   2    0.01    1.21848                                                                                                 */
/*   3    0.02    1.16561                                                                                                 */
/*   4    0.03    1.13006                                                                                                 */
/*   5    0.04    1.13513                                                                                                 */
/* ...                                                                                                                    */
/* 311    3.10    1.05822                                                                                                 */
/* 312    3.11    1.13153                                                                                                 */
/* 313    3.12    1.05568                                                                                                 */
/* 314    3.13    1.04113                                                                                                 */
/* 315    3.14    1.06199                                                                                                 */
/*                                                                                                                        */
/*  0.0       1.1   X   2.2       3.3                                                                                     */
/*  -+---------+---------+---------+-                                                                                     */
/* 3+                               +3                                                                                    */
/*  | y=sin(2*x)+1.1+ normal(0,.05) |                                                                                     */
/*  |                               |                                                                                     */
/*  |      ***                      |                                                                                     */
/* 2+    *******                    +2                                                                                    */
/*  |  ***     ***                  |                                                                                     */
/* Y| ***        **                 |Y                                                                                    */
/*  |***          ***            *  |                                                                                     */
/* 1+*             **            ** +1                                                                                    */
/*  |               ***        ***  |                                                                                     */
/*  |                 **      **    |                                                                                     */
/*  |                  ********     |                                                                                     */
/* 0 +                   *****     0+                                                                                     */
/*  |                               |                                                                                     */
/*  -+---------+---------+---------+-                                                                                     */
/*  0.0       1.1   X   2.2       3.3                                                                                     */
/**************************************************************************************************************************/

/*               _                                _                _       _
/ |   ___  _   _| |_ ___ _ __ _ __ ___   ___  ___| |_  _ __   ___ (_)_ __ | |_ ___
| |  / _ \| | | | __/ _ \ `__| `_ ` _ \ / _ \/ __| __|| `_ \ / _ \| | `_ \| __/ __|
| | | (_) | |_| | ||  __/ |  | | | | | | (_) \__ \ |_ | |_) | (_) | | | | | |_\__ \
|_|  \___/ \__,_|\__\___|_|  |_| |_| |_|\___/|___/\__|| .__/ \___/|_|_| |_|\__|___/
                                                      |_|
*/

%utlfkil("d:/png/hul.png")

%utl_rbeginx;
parmcards4;
# Load required libraries
library(ggplot2)
library(dplyr)
library(haven)
source("c:/oto/fn_tosas9x.R")
have<-read_sas("d:/sd1/have.sas7bdat")
x=have$X;
y<-have$Y;
library(ggplot2)
library(dplyr)

# Step 1: Generate noisy sine wave data
set.seed(123) # For reproducibility
x <- seq(0, 2 * pi, length.out = 500)
y <- sin(x) + rnorm(length(x), sd = 0.2)

# Combine x and y into a data frame
data <- data.frame(x = x, y = y)
# Load required libraries

upper_hull_indices <- chull(data$x, data$y)
upper_hull <-
  data[upper_hull_indices, ] %>% arrange(x)

upper_hull

png("d:/png/hul.png");

ggplot(data, aes(x, y)) +
  geom_point(alpha = 0.5) +
  geom_point(
    data = upper_hull
   ,aes(x, y)
   ,color = "red"
   ,size = 5) +
  labs(title="Convex Hull Outermost points",
       x = "X",
       y = "Y") +
  theme_minimal()
dev.off()
fn_tosas9x(
      inp    = upper_hull
     ,outlib ="d:/sd1/"
     ,outdsn ="upr"
     )
;;;;
%utl_rendx;

proc print data=sd1.upr;
run;quit;


/**************************************************************************************************************************/
/* R OUTERMOST POINTS       |    SAS                               |      https://tinyurl.com/3492zs9d                    */
/*                          |                                      |                                                      */
/*      X          Y        |    ROWNAMES       X           Y      |    0.0       1.1  X    2.2     3.3                   */
/*                          |                                      |    ----------+---------+---------+-                  */
/* 0.00000000 -0.1120951    |        1       0.00000    -0.11210   |    |                              |                  */
/* 0.02518311  0.3369221    |        2       0.02518     0.33692   |    |Convex hulls                  |                  */
/* 0.06295777  0.4059292    |        3       0.06296     0.40593   |    |                              |                  */
/* 0.08814088 -0.1649855    |        4       0.08814    -0.16499   |    |All (x,y) points between lines|                  */
/* 0.54143681  0.9491590    |        5       0.54144     0.94916   |    |                              |                  */
/* 0.86881721  1.1735826    |        6       0.86882     1.17358   |   3+     /*                       +3                 */
/* 1.20878916  1.3726545    |        7       1.20879     1.37265   |    |    / * *                     |                  */
/* 2.05242326  1.5344505    |        8       2.05242     1.53445   |   Y|   |** ****                   |Y                 */
/* 4.50777623 -1.4723194    |        9       4.50778    -1.47232   |    |   \* *    *                  |                  */
/* 5.22549479 -1.3998539    |       10       5.22549    -1.39985   |   2+   * **    **                 +2                 */
/* 5.72915694 -1.0583018    |       11       5.72916    -1.05830   |    |  /  *      **           */   |                  */
/* 6.18245288  0.1802479    |       12       6.18245     0.18025   |    | * *         **         */    |                  */
/* 6.23281909 -0.3219607    |       13       6.23282    -0.32196   |    |              *         *     |                  */
/* 6.28318531  0.1104315    |       14       6.28319     0.11043   |   1+               **       *\    +1                 */
/*                          |                                      |    |                 *    **/     |                  */
/*                          |                                      |    |                  ** * /      |                  */
/*                          |                                      |    |                     *        |                  */
/*                          |                                      |   0+                              +0                 */
/*                          |                                      |    ----------+---------+---------+-                  */
/*                          |                                      |    0.0       1.1   X   2.2     3.3                   */
/*                          |                                      |                                                      */
/*                          |                                      |                                                      */
/**************************************************************************************************************************/

/*___             _       _           _
|___ \   _ __ ___| | __ _| |_ ___  __| |  _ __ ___ _ __   ___  ___
  __) | | `__/ _ \ |/ _` | __/ _ \/ _` | | `__/ _ \ `_ \ / _ \/ __|
 / __/  | | |  __/ | (_| | ||  __/ (_| | | | |  __/ |_) | (_) \__ \
|_____| |_|  \___|_|\__,_|\__\___|\__,_| |_|  \___| .__/ \___/|___/
                                                  |_|
*/


https://github.com/rogerjdeangelis/utl-draw-polygons-around-clusters-of-points-r-convex-hulls
https://github.com/rogerjdeangelis/utl_convex_hull_maximum-distance-between-two-points-in-a-scatter-plot
https://github.com/rogerjdeangelis/utl_convex_hull_polygons_encompassing_a_three_dimensional_scatter_plot
https://github.com/rogerjdeangelis/utl_simple_convex_hull_polygon_envelop_for_a_scatter_plot
https://github.com/rogerjdeangelis/utl_AI_extracting_and_saving_video_frames
https://github.com/rogerjdeangelis/utl_AI_is_it_a_license_plate
https://github.com/rogerjdeangelis/utl_AI_minimum_distance_from_a_poin_to_a_polygon
https://github.com/rogerjdeangelis/utl-area-between-curves-with-an-intersection-point-adding-negative-and-positive-areas-plot-sympy
https://github.com/rogerjdeangelis/utl-distance-between-a-point-and-curve-in-sql-and-wps-pythony-r-sympy
https://github.com/rogerjdeangelis/utl-python-sympy-projection-of-the-intersection-of-two-parabolic-surfaces-onto-the-xy-plane-AI
https://github.com/rogerjdeangelis/utl-r-python-compute-the-area-between-two-curves-AI-sympy-trapezoid

/*              _
  ___ _ __   __| |
 / _ \ `_ \ / _` |
|  __/ | | | (_| |
 \___|_| |_|\__,_|

*/

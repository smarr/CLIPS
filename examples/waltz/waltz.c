
#include "clips.h"

#include <math.h>

/* waltz */

void make_3_junction(void);
float get_angle(int,int);
float inscribed_angle(int,int,int);

#define PI      3.1415927
#define MOD_NUM      100
#define get_y(val)   (val % MOD_NUM)
#define get_x(val)   ((int) (val / MOD_NUM))

/*
   DefineFunction2("make_3_junction",'v', PTIF make_3_junction,"make_3_junction","44");
*/

/********************************************************************/
/* get_angle: This function is passed two points and calculates the */
/*   angle between the line defined by these points and the x-axis. */
/********************************************************************/
float get_angle(
  int p1,
  int p2)
  {
   int delta_x, delta_y;

   /* Calculate (x2 - x1) and (y2 - y1).  The points are passed in the
    * form x1y1 and x2y2.  get_x() and get_y() are passed these points
    * and return the x and y values respectively.  For example,
    * get_x(1020) returns 10. */
   
   delta_x = get_x(p2) - get_x(p1);
   delta_y = get_y(p2) - get_y(p1);

   if (delta_x == 0) 
     {
      if (delta_y > 0)
        { return(PI/2); }
      else if (delta_y < 0)
        { return(-PI/2); }
     }
   else if (delta_y == 0) 
     {
      if (delta_x > 0)
        { return(0.0); }
      else if (delta_x < 0)
        { return(PI); }
     }

   return((float) atan2((double) delta_y,(double) delta_x));
  }


/**********************************************************************
 * This procedure is passed the basepoint of the intersection of two lines
 * as well as the other two endpoints of the lines and calculates the
 * angle inscribed by these three points.
 **********************************************************************/
float
inscribed_angle(
  int basepoint, 
  int p1, 
  int p2)
{
   float angle1, angle2, temp;

   /* Get the angle between line #1 and the origin and the angle
    * between line #2 and the origin, and then subtract these values. */
   angle1 = get_angle(basepoint,p1);
   angle2 = get_angle(basepoint,p2);
   temp = angle1 - angle2;
   if (temp < 0.0)
      temp = -temp;

   /* We always want the smaller of the two angles inscribed, so if the
    * answer is greater than 180 degrees, calculate the smaller angle and
    * return it. */
   if (temp > PI)
      temp = 2*PI - temp;
   if (temp < 0.0)
      return(-temp);
   return(temp);
}


void
make_3_junction()
{
   char buffer[300];

   int basepoint,p1,p2,p3;
   int shaft,barb1,barb2;
   float angle12, angle13, angle23;
   float sum, sum1213, sum1223, sum1323;
   float delta;
    char *type;

   basepoint = (int) RtnLong(1);
   p1 = (int) RtnLong(2);
   p2 = (int) RtnLong(3);
   p3 = (int) RtnLong(4);

   angle12 = inscribed_angle(basepoint,p1,p2);
   angle13 = inscribed_angle(basepoint,p1,p3);
   angle23 = inscribed_angle(basepoint,p2,p3);

   sum1213 = angle12 + angle13;
   sum1223 = angle12 + angle23;
   sum1323 = angle13 + angle23;

   if (sum1213 < sum1223) {
      if (sum1213 < sum1323) {
         sum = sum1213;
         shaft = p1; barb1 = p2; barb2 = p3;
      }
      else {
         sum = sum1323;
         shaft = p3; barb1 = p1; barb2 = p2;
      }
   }
   else {
      if (sum1223 < sum1323) {
         sum = sum1223;
         shaft = p2; barb1 = p1; barb2 = p3;
      }
      else {
         sum = sum1323;
         shaft = p3; barb1 = p1; barb2 = p2;
      }
   }

   delta = sum - PI;
   if (delta < 0.0)
      delta = -delta;

   if (delta < 0.001)
      type = "tee";
   else if (sum > PI)
      type = "fork";
   else
      type = "arrow";

   sprintf(buffer,"(junction (p1 %d) (p2 %d) (p3 %d) (base_point %d) (type %s))",
                   barb1,shaft,barb2, basepoint,type);
    
   AssertString(buffer);
}


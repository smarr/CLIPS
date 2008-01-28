package CLIPSJNI;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class MultifieldValue extends PrimitiveValue
  {
   /********************/
   /* MultifieldValue: */
   /********************/
   public MultifieldValue()
     {
      super(new ArrayList());
     }

   /********************/
   /* MultifieldValue: */
   /********************/
   public MultifieldValue(
     List value)
     {
      super(value);
     }
     
   /**************/
   /* listValue: */
   /**************/
   public List listValue()
     {
      return (List) getValue();
     }
     
   /*************/
   /* toString: */
   /*************/
   public String toString()
     {  
      List theList = (List) getValue();
      boolean first = true;
      
      String theString = "(";
      
      for (Iterator itr = theList.iterator(); itr.hasNext(); ) 
        {
         if (! first)
          { theString = theString + " " + itr.next(); }
         else
          { 
           theString = theString + itr.next(); 
           first = false;
          }
        }      
        
      theString = theString + ")";
      
      return theString;
     }
  }

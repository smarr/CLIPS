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
     
   /********************/
   /* multifieldValue: */
   /********************/
   public List multifieldValue() throws Exception
     {
      return (List) getValue();
     }

   /********************/
   /* get: */
   /********************/
   public PrimitiveValue get(
     int index) throws Exception
     {
      List theList = (List) getValue();
      
      return (PrimitiveValue) theList.get(index);
     }
     
   /********************/
   /* size: */
   /********************/
   public int size() throws Exception
     {
      List theList = (List) getValue();
      
      return theList.size();
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

package CLIPSJNI;

public class SymbolValue extends PrimitiveValue
  {
   /****************/
   /* SymbolValue: */
   /****************/
   public SymbolValue()
     {
      super(new String(""));
     }

   /****************/
   /* SymbolValue: */
   /****************/
   public SymbolValue(
     String value)
     {
      super(value);
     }

   /****************/
   /* stringValue: */
   /****************/
   public String stringValue()
     {
      return (String) getValue();
     }
  }

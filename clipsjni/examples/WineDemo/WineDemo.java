import javax.swing.*; 
import javax.swing.border.*; 
import javax.swing.table.*;
import java.awt.*; 
import java.awt.event.*; 
 
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.MissingResourceException;

import CLIPSJNI.*;

/* TBD module qualifier with find-all-facts */

class WineDemo implements ActionListener
  {  
   JFrame jfrm;
   
   DefaultTableModel wineList;
  
   JComboBox preferredColor; 
   JComboBox preferredBody; 
   JComboBox preferredSweetness; 

   JComboBox mainCourse; 
   JComboBox sauce; 
   JComboBox flavor; 
   
   JLabel jlab; 

   String preferredColorNames[] = { "Don't Care", "Red", "White" }; 
   String preferredBodyNames[] = { "Don't Care", "Light", "Medium", "Full" }; 
   String preferredSweetnessNames[] = { "Don't Care", "Dry", "Medium", "Sweet" }; 
   
   String mainCourseNames[] = { "Don't Know", "Beef", "Pork", "Lamb", "Turkey", "Chicken", "Duck", "Fish", "Other" };
   String sauceNames[] = { "Don't Know", "None", "Spicy", "Sweet", "Cream", "Other" };
   String flavorNames[] = { "Don't Know", "Delicate", "Average", "Strong" };
 
   String preferredColorChoices[] = new String[3]; 
   String preferredBodyChoices[] = new String[4]; 
   String preferredSweetnessChoices[] = new String[4]; 
   
   String mainCourseChoices[] = new String[9];
   String sauceChoices[] = new String[6];
   String flavorChoices[] = new String[4];

   ResourceBundle wineResources;

   Environment clips;
   
   boolean isExecuting = false;
   Thread executionThread;

   class WeightCellRenderer extends JProgressBar implements TableCellRenderer 
     {
      public WeightCellRenderer() 
        {
         super(JProgressBar.HORIZONTAL,0,100);
         setStringPainted(false);
        }
  
      public Component getTableCellRendererComponent(
        JTable table, 
        Object value,
        boolean isSelected, 
        boolean hasFocus, 
        int row, 
        int column) 
        { 
         setValue(((Number) value).intValue());
         return WeightCellRenderer.this; 
        }
     }
      
   /************/
   /* WineDemo */
   /************/
   WineDemo()
     {  
      try
        {
         wineResources = ResourceBundle.getBundle("resources.WineResources",Locale.getDefault());
        }
      catch (MissingResourceException mre)
        {
         mre.printStackTrace();
         return;
        }

      preferredColorChoices[0] = wineResources.getString("Don'tCare"); 
      preferredColorChoices[1] = wineResources.getString("Red"); 
      preferredColorChoices[2] = wineResources.getString("White"); 
      
      preferredBodyChoices[0] = wineResources.getString("Don'tCare"); 
      preferredBodyChoices[1] = wineResources.getString("Light"); 
      preferredBodyChoices[2] = wineResources.getString("MediumBody"); 
      preferredBodyChoices[3] = wineResources.getString("Full"); 

      preferredSweetnessChoices[0] = wineResources.getString("Don'tCare"); 
      preferredSweetnessChoices[1] = wineResources.getString("Dry"); 
      preferredSweetnessChoices[2] = wineResources.getString("MediumSweetness"); 
      preferredSweetnessChoices[3] = wineResources.getString("Sweet"); 
      
      mainCourseChoices[0] = wineResources.getString("Don'tKnow"); 
      mainCourseChoices[1] = wineResources.getString("Beef"); 
      mainCourseChoices[2] = wineResources.getString("Pork"); 
      mainCourseChoices[3] = wineResources.getString("Lamb"); 
      mainCourseChoices[4] = wineResources.getString("Turkey"); 
      mainCourseChoices[5] = wineResources.getString("Chicken"); 
      mainCourseChoices[6] = wineResources.getString("Duck"); 
      mainCourseChoices[7] = wineResources.getString("Fish"); 
      mainCourseChoices[8] = wineResources.getString("Other"); 
   
      sauceChoices[0] = wineResources.getString("Don'tKnow"); 
      sauceChoices[1] = wineResources.getString("None"); 
      sauceChoices[2] = wineResources.getString("Spicy"); 
      sauceChoices[3] = wineResources.getString("Sweet"); 
      sauceChoices[4] = wineResources.getString("Cream"); 
      sauceChoices[5] = wineResources.getString("Other"); 

      flavorChoices[0] = wineResources.getString("Don'tKnow"); 
      flavorChoices[1] = wineResources.getString("Delicate"); 
      flavorChoices[2] = wineResources.getString("Average"); 
      flavorChoices[3] = wineResources.getString("Strong"); 

      /*===================================*/
      /* Create a new JFrame container and */
      /* assign a layout manager to it.    */
      /*===================================*/
     
      jfrm = new JFrame(wineResources.getString("WineDemo"));          
      jfrm.getContentPane().setLayout(new BoxLayout(jfrm.getContentPane(),BoxLayout.Y_AXIS));
    
      /*=================================*/
      /* Give the frame an initial size. */
      /*=================================*/
     
      jfrm.setSize(480,390);  
  
      /*=============================================================*/
      /* Terminate the program when the user closes the application. */
      /*=============================================================*/
     
      jfrm.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);  
 
      /*===============================*/
      /* Create the preferences panel. */
      /*===============================*/
      
      JPanel preferencesPanel = new JPanel(); 
      GridLayout theLayout = new GridLayout(3,2);
      preferencesPanel.setLayout(theLayout);   
      preferencesPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder(),
                                                                 wineResources.getString("PreferencesTitle"),
                                                                 TitledBorder.CENTER,
                                                                 TitledBorder.ABOVE_TOP));
 
      preferencesPanel.add(new JLabel(wineResources.getString("ColorLabel")));
      preferredColor = new JComboBox(preferredColorChoices); 
      preferencesPanel.add(preferredColor);
      preferredColor.addActionListener(this);
     
      preferencesPanel.add(new JLabel(wineResources.getString("BodyLabel")));
      preferredBody = new JComboBox(preferredBodyChoices); 
      preferencesPanel.add(preferredBody);
      preferredBody.addActionListener(this);

      preferencesPanel.add(new JLabel(wineResources.getString("SweetnessLabel")));
      preferredSweetness = new JComboBox(preferredSweetnessChoices); 
      preferencesPanel.add(preferredSweetness);
      preferredSweetness.addActionListener(this);

      /*========================*/
      /* Create the meal panel. */
      /*========================*/
     
      JPanel mealPanel = new JPanel(); 
      theLayout = new GridLayout(3,2);
      mealPanel.setLayout(theLayout);   
      mealPanel.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder(),
                                                                 wineResources.getString("MealTitle"),
                                                                 TitledBorder.CENTER,
                                                                 TitledBorder.ABOVE_TOP));
 
      mealPanel.add(new JLabel(wineResources.getString("MainCourseLabel")));
      mainCourse = new JComboBox(mainCourseChoices); 
      mealPanel.add(mainCourse);
      mainCourse.addActionListener(this);
    
      mealPanel.add(new JLabel(wineResources.getString("SauceLabel")));
      sauce = new JComboBox(sauceChoices); 
      mealPanel.add(sauce);
      sauce.addActionListener(this);

      mealPanel.add(new JLabel(wineResources.getString("FlavorLabel")));
      flavor = new JComboBox(flavorChoices); 
      mealPanel.add(flavor);
      flavor.addActionListener(this);
      
      /*==============================================*/
      /* Create a panel including the preferences and */
      /* meal panels and add it to the content pane.  */
      /*==============================================*/

      JPanel choicesPanel = new JPanel(); 
      choicesPanel.setLayout(new FlowLayout());
      choicesPanel.add(preferencesPanel);
      choicesPanel.add(mealPanel);
      
      jfrm.getContentPane().add(choicesPanel); 
 
      /*==================================*/
      /* Create the recommendation panel. */
      /*==================================*/

      wineList = new DefaultTableModel();

      wineList.setDataVector(new Object[][] { },
                             new Object[] { wineResources.getString("WineTitle"), 
                                            wineResources.getString("RecommendationTitle")});
         
      JTable table = 
         new JTable(wineList)
           {
            public boolean isCellEditable(int rowIndex,int vColIndex) 
              { return false; }
           };

      table.setCellSelectionEnabled(false); 

      WeightCellRenderer renderer = this.new WeightCellRenderer(); 
      renderer.setBackground(table.getBackground());

      table.getColumnModel().getColumn(1).setCellRenderer(renderer);

      JScrollPane pane = new JScrollPane(table);
    
      table.setPreferredScrollableViewportSize(new Dimension(450,210)); 
        
      /*===================================================*/
      /* Add the recommendation panel to the content pane. */
      /*===================================================*/

      jfrm.getContentPane().add(pane); 

      /*===================================================*/
      /* Initially select the first item in each ComboBox. */
      /*===================================================*/
       
      preferredColor.setSelectedIndex(0); 
      preferredBody.setSelectedIndex(0); 
      preferredSweetness.setSelectedIndex(0); 
      mainCourse.setSelectedIndex(0);
      sauce.setSelectedIndex(0);
      flavor.setSelectedIndex(0);

      /*========================*/
      /* Load the wine program. */
      /*========================*/
      
      clips = new Environment();
      
      clips.load("winedemo.clp");
      
      try
        { runWine(); }
      catch (Exception e)
        { e.printStackTrace(); }
       
      /*====================*/
      /* Display the frame. */
      /*====================*/

      jfrm.pack();
      jfrm.setVisible(true);  
     }  
 
   /*########################*/
   /* ActionListener Methods */
   /*########################*/

   /*******************/
   /* actionPerformed */
   /*******************/  
   public void actionPerformed(
     ActionEvent ae) 
     { 
      if (clips == null) return;
      
      try
        { runWine(); }
      catch (Exception e)
        { e.printStackTrace(); }
     }
     
   /***********/
   /* runWine */
   /***********/  
   private void runWine() throws Exception
     { 
      String item;
      
      if (isExecuting) return;
      
      clips.reset();      
            
      item = preferredColorNames[preferredColor.getSelectedIndex()];
      
      if (item.equals("Red"))   
        { clips.assertString("(attribute (name preferred-color) (value red))"); }
      else if (item.equals("White"))   
        { clips.assertString("(attribute (name preferred-color) (value white))"); }
      else
        { clips.assertString("(attribute (name preferred-color) (value unknown))"); }

      item = preferredBodyNames[preferredBody.getSelectedIndex()];
      if (item.equals("Light"))   
        { clips.assertString("(attribute (name preferred-body) (value light))"); }
      else if (item.equals("Medium"))   
        { clips.assertString("(attribute (name preferred-body) (value medium))"); }
      else if (item.equals("Full"))   
        { clips.assertString("(attribute (name preferred-body) (value full))"); }
      else
        { clips.assertString("(attribute (name preferred-body) (value unknown))"); }
 
      item = preferredSweetnessNames[preferredSweetness.getSelectedIndex()];
      if (item.equals("Dry"))   
        { clips.assertString("(attribute (name preferred-sweetness) (value dry))"); }
      else if (item.equals("Medium"))   
        { clips.assertString("(attribute (name preferred-sweetness) (value medium))"); }
      else if (item.equals("Sweet"))   
        { clips.assertString("(attribute (name preferred-sweetness) (value sweet))"); }
      else
        { clips.assertString("(attribute (name preferred-sweetness) (value unknown))"); }

      item = mainCourseNames[mainCourse.getSelectedIndex()];
      if (item.equals("Beef") ||
          item.equals("Pork") ||
          item.equals("Lamb"))
        { 
         clips.assertString("(attribute (name main-component) (value meat))"); 
         clips.assertString("(attribute (name has-turkey) (value no))");
        }
      else if (item.equals("Turkey"))   
        { 
         clips.assertString("(attribute (name main-component) (value poultry))"); 
         clips.assertString("(attribute (name has-turkey) (value yes))");
        }
      else if (item.equals("Chicken") ||
               item.equals("Duck"))   
        { 
         clips.assertString("(attribute (name main-component) (value poultry))"); 
         clips.assertString("(attribute (name has-turkey) (value no))");
        }
      else if (item.equals("Fish"))   
        { 
         clips.assertString("(attribute (name main-component) (value fish))"); 
         clips.assertString("(attribute (name has-turkey) (value no))");
        }
      else if (item.equals("Other"))   
        { 
         clips.assertString("(attribute (name main-component) (value unknown))"); 
         clips.assertString("(attribute (name has-turkey) (value no))");
        }
      else
        { 
         clips.assertString("(attribute (name main-component) (value unknown))"); 
         clips.assertString("(attribute (name has-turkey) (value unknown))");
        }

      item = sauceNames[sauce.getSelectedIndex()];
      if (item.equals("None"))   
        { clips.assertString("(attribute (name has-sauce) (value no))"); }
      else if (item.equals("Spicy"))   
        { 
         clips.assertString("(attribute (name has-sauce) (value yes))");
         clips.assertString("(attribute (name sauce) (value spicy))");
        }
      else if (item.equals("Sweet"))   
        { 
         clips.assertString("(attribute (name has-sauce) (value yes))");
         clips.assertString("(attribute (name sauce) (value sweet))");
        }
      else if (item.equals("Cream"))   
        { 
         clips.assertString("(attribute (name has-sauce) (value yes))");
         clips.assertString("(attribute (name sauce) (value cream))");
        }
      else if (item.equals("Other"))   
        { 
         clips.assertString("(attribute (name has-sauce) (value yes))");
         clips.assertString("(attribute (name sauce) (value unknown))");
        }
      else
        { 
         clips.assertString("(attribute (name has-sauce) (value unknown))");
         clips.assertString("(attribute (name sauce) (value unknown))");
        }

      item = flavorNames[flavor.getSelectedIndex()];
      if (item.equals("Delicate"))   
        { clips.assertString("(attribute (name tastiness) (value delicate))"); }
      else if (item.equals("Average"))   
        { clips.assertString("(attribute (name tastiness) (value average))"); }
      else if (item.equals("Strong"))   
        { clips.assertString("(attribute (name tastiness) (value strong))"); }
      else
        { clips.assertString("(attribute (name tastiness) (value unknown))"); }
      
      Runnable runThread = 
         new Runnable()
           {
            public void run()
              {
               clips.run();
               
               SwingUtilities.invokeLater(
                  new Runnable()
                    {
                     public void run()
                       {
                        try 
                          { updateWines(); }
                        catch (Exception e)
                          { e.printStackTrace(); }
                       }
                    });
              }
           };
      
      isExecuting = true;
      
      executionThread = new Thread(runThread);
      
      executionThread.start();
     }
     
   /***************/
   /* updateWines */
   /***************/  
   private void updateWines() throws Exception
     { 
      String evalStr = "(WINES::get-wine-list)";
                                       
      PrimitiveValue pv = clips.eval(evalStr);
               
      wineList.setRowCount(0);
      
      for (int i = 0; i < pv.size(); i++) 
        {
         PrimitiveValue fv = pv.get(i);

         int certainty = fv.getFactSlot("certainty").numberValue().intValue(); 
         
         String wineName = fv.getFactSlot("value").stringValue();
                  
         wineList.addRow(new Object[] { wineName, new Integer(certainty) });
        }  
        
      jfrm.pack();
      
      executionThread = null;
      
      isExecuting = false;
     }     
     
   /********/
   /* main */
   /********/  
   public static void main(String args[])
     {  
      /*===================================================*/
      /* Create the frame on the event dispatching thread. */
      /*===================================================*/
      
      SwingUtilities.invokeLater(
        new Runnable() 
          {  
           public void run() { new WineDemo(); }  
          });   
     }  
  }
import javax.swing.*; 
import javax.swing.border.*; 
import javax.swing.table.*;
import java.awt.*; 
import java.awt.event.*; 
 
import java.util.Iterator;
import java.util.List;
 
import CLIPSJNI.*;

/* TBD module qualifier with find-all-facts */

class WineDemo implements ActionListener
  {  
   DefaultTableModel wineList;
  
   JComboBox preferredColor; 
   JComboBox preferredBody; 
   JComboBox preferredSweetness; 

   JComboBox mainCourse; 
   JComboBox sauce; 
   JComboBox flavor; 
   
   JLabel jlab; 
 
   String preferredColorChoices[] = { "Don't Care", "Red", "White" }; 
   String preferredBodyChoices[] = { "Don't Care", "Light", "Medium", "Full" }; 
   String preferredSweetnessChoices[] = { "Don't Care", "Dry", "Medium", "Sweet" }; 
   
   String mainCourseChoices[] = { "Don't Know", "Beef", "Pork", "Lamb", "Turkey", "Chicken", "Duck", "Fish", "Other" };
   String sauceChoices[] = { "Don't Know", "None", "Spicy", "Sweet", "Cream", "Other" };
   String flavorChoices[] = { "Don't Know", "Delicate", "Average", "Strong" };

   Environment clips;

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
      
   WineDemo()
     {  
      /*================================*/
      /* Create a new JFrame container. */
      /*================================*/
     
      JFrame jfrm = new JFrame("Wine Demo");  
 
      /*=============================*/
      /* Specify FlowLayout manager. */
      /*=============================*/
        
      jfrm.getContentPane().setLayout(new FlowLayout());  
 
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
                                                                 "Preferences",
                                                                 TitledBorder.CENTER,
                                                                 TitledBorder.ABOVE_TOP));
 
      preferencesPanel.add(new JLabel("Color:"));
      preferredColor = new JComboBox(preferredColorChoices); 
      preferencesPanel.add(preferredColor);
      preferredColor.addActionListener(this);
     
      preferencesPanel.add(new JLabel("Body:"));
      preferredBody = new JComboBox(preferredBodyChoices); 
      preferencesPanel.add(preferredBody);
      preferredBody.addActionListener(this);

      preferencesPanel.add(new JLabel("Sweetness:"));
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
                                                                 "Meal",
                                                                 TitledBorder.CENTER,
                                                                 TitledBorder.ABOVE_TOP));
 
      mealPanel.add(new JLabel("Main Course:"));
      mainCourse = new JComboBox(mainCourseChoices); 
      mealPanel.add(mainCourse);
      mainCourse.addActionListener(this);
    
      mealPanel.add(new JLabel("Sauce:"));
      sauce = new JComboBox(sauceChoices); 
      mealPanel.add(sauce);
      sauce.addActionListener(this);

      mealPanel.add(new JLabel("Flavor:"));
      flavor = new JComboBox(flavorChoices); 
      mealPanel.add(flavor);
      flavor.addActionListener(this);
      
      /*==================================*/
      /* Create the recommendation panel. */
      /*==================================*/

      wineList = new DefaultTableModel();

      wineList.setDataVector(new Object[][] { },
                             new Object[] { "Wine", "Recommendation Weight"});
         
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
        
      /*==================================================*/
      /* Add the two ComboBox panels to the content pane. */
      /*==================================================*/
      
      jfrm.getContentPane().add(preferencesPanel); 
      jfrm.getContentPane().add(mealPanel); 
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
      
      runWine();
       
      /*====================*/
      /* Display the frame. */
      /*====================*/
      
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
      
      runWine();
     }
     
   /*******************/
   /* runWine */
   /*******************/  
   private void runWine() 
     { 
      String item;
      clips.reset();      
            
      item = (String) preferredColor.getSelectedItem();
      if (item.equals("Red"))   
        { clips.assertString("(attribute (name preferred-color) (value red))"); }
      else if (item.equals("White"))   
        { clips.assertString("(attribute (name preferred-color) (value white))"); }
      else
        { clips.assertString("(attribute (name preferred-color) (value unknown))"); }

      item = (String) preferredBody.getSelectedItem();
      if (item.equals("Light"))   
        { clips.assertString("(attribute (name preferred-body) (value light))"); }
      else if (item.equals("Medium"))   
        { clips.assertString("(attribute (name preferred-body) (value medium))"); }
      else if (item.equals("Full"))   
        { clips.assertString("(attribute (name preferred-body) (value full))"); }
      else
        { clips.assertString("(attribute (name preferred-body) (value unknown))"); }
 
      item = (String) preferredSweetness.getSelectedItem();
      if (item.equals("Dry"))   
        { clips.assertString("(attribute (name preferred-sweetness) (value dry))"); }
      else if (item.equals("Medium"))   
        { clips.assertString("(attribute (name preferred-sweetness) (value medium))"); }
      else if (item.equals("Sweet"))   
        { clips.assertString("(attribute (name preferred-sweetness) (value sweet))"); }
      else
        { clips.assertString("(attribute (name preferred-sweetness) (value unknown))"); }

      item = (String) mainCourse.getSelectedItem();
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

      item = (String) sauce.getSelectedItem();
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

      item = (String) flavor.getSelectedItem();
      if (item.equals("Delicate"))   
        { clips.assertString("(attribute (name tastiness) (value delicate))"); }
      else if (item.equals("Average"))   
        { clips.assertString("(attribute (name tastiness) (value average))"); }
      else if (item.equals("Strong"))   
        { clips.assertString("(attribute (name tastiness) (value strong))"); }
      else
        { clips.assertString("(attribute (name tastiness) (value unknown))"); }
      
      clips.run();
      
      String evalStr = "(WINES::get-wine-list)";
                                             
      MultifieldValue pv = (MultifieldValue) clips.eval(evalStr);
            
      List theList = pv.listValue();
      
      wineList.setRowCount(0);
      
      for (Iterator itr = theList.iterator(); itr.hasNext(); ) 
        {
         FactAddressValue fv = (FactAddressValue) itr.next();

         int certainty = ((FloatValue) fv.getFactSlot("certainty")).intValue(); 
         
         String wineName = ((StringValue) fv.getFactSlot("value")).stringValue();
                  
         wineList.addRow(new Object[] { wineName, new Integer(certainty) });

        }      
     }
     
   public static void main(String args[])
     {  
      // Create the frame on the event dispatching thread.  
      SwingUtilities.invokeLater(
        new Runnable() 
          {  
           public void run() { new WineDemo(); }  
          });   
     }  
  }
/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package chinese_paper;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Scanner;

/**
 *
 * @author Mikaela
 */
public class Chinese_paper {

    public static ArrayList<GOs> GO_categories_with_ids = new ArrayList();
    public static Hashtable<String, String> give_number_get_group = new Hashtable();
    public static Hashtable<String, String> give_group_get_number = new Hashtable();
    public static Hashtable<String, GOs> give_number_get_GO_object = new Hashtable();
    public static String filename_all_info = "4B_Figure.txt";
    public static String filename_group_info = "4B_group_map.txt";
    public static StringBuffer buf = new StringBuffer();

    public static void main(String[] args) {
        try {
            File myObj = new File(filename_group_info);
            Scanner myReader = new Scanner(myObj);
            while (myReader.hasNextLine()) {
                String data = myReader.nextLine();
                String[] str = data.split("\t");
                give_number_get_group.put(str[0], str[1]);
                give_group_get_number.put(str[1], str[0]);
                GOs ggg = new GOs();
                ggg.GO_number = str[0];
                ggg.GO_name = str[1];
                ggg.go_terms = new ArrayList();
                GO_categories_with_ids.add(ggg);
                give_number_get_GO_object.put(str[0], ggg);

            }
            myReader.close();
        } catch (FileNotFoundException e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }

        ////////////////////////////////////////////////////
        try {
            File myObj = new File(filename_all_info);
            Scanner myReader = new Scanner(myObj);
            int cnt = 0;
            while (myReader.hasNextLine()) {
                String data = myReader.nextLine();
                if (cnt != 0) {
                    String[] str = data.split("\t");
                    String group_number = str[0];
                    String GO = str[1].split("~")[0];
                    GOs go = give_number_get_GO_object.get(group_number);
//                    System.out.println(group_number);
//                    System.out.println(go.GO_number);
                    if (!go.go_terms.contains(GO)) {
                        go.go_terms.add(GO);
                    }
                }
                cnt++;
            }
            myReader.close();
        } catch (FileNotFoundException e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }
        
        
        
        for (int i = 1; i < GO_categories_with_ids.size(); i++) {
            GOs gg = GO_categories_with_ids.get(i);
            buf.append(gg.GO_name +"\t" +gg.go_terms.toString().replaceAll(" ", "").replace("[", "").replace("]", "")+"\n");
        }

        write_to_file(buf,"4B_annotation_file_NORMA.txt");
        
    }

    static void write_to_file(StringBuffer buf, String filename) {
        try {
            FileWriter myWriter = new FileWriter(filename);
            myWriter.write(buf.toString());
            System.out.println("Wrote file successfully");
            myWriter.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

}

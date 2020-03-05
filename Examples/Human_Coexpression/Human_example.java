/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Hashtable;

/**
 *
 * @author George
 */
public class Human_example {

    static Hashtable<String, String> give_node_get_name = new Hashtable();
    static Hashtable<String, ArrayList<String>> give_annotation_get_genes = new Hashtable();
    static String file = "C:\\Users\\George\\Desktop\\Human_Coexpression\\Human_coexpression_nodes.txt";
    static String network_file = "C:\\Users\\George\\Desktop\\Human_Coexpression\\Human_coexpression_NETWORK.txt";
    static String kegg_file = "C:\\Users\\George\\Desktop\\Human_Coexpression\\Human_coexpression_KEGG.txt";
    static String go_MF_file = "C:\\Users\\George\\Desktop\\Human_Coexpression\\Human_coexpression_GO_MF.txt";
    static String go_BP_file = "C:\\Users\\George\\Desktop\\Human_Coexpression\\Human_coexpression_GO_BP.txt";
    static String go_CC_file = "C:\\Users\\George\\Desktop\\Human_Coexpression\\Human_coexpression_GO_CC.txt";
    static int elements_allowed = 3;

    public static void main(String[] args) {
        read_network(network_file);
        read_KEGG(file);
        read_GO_CC(file);
        read_GO_BP(file);
        read_GO_MF(file);
    }

    static void read_network(String fileName) {
        String line = null;
        try {
            int cnt = 0;
            FileReader fileReader = new FileReader(fileName);
            BufferedReader bufferedReader = new BufferedReader(fileReader);
            while ((line = bufferedReader.readLine()) != null) {
                String[] str = line.split("\t");
                if (cnt != 0) {
                    {
                        String[] sss = line.split("\t");
                        give_node_get_name.put(sss[0], sss[0]);
                        give_node_get_name.put(sss[1], sss[1]);
                    }
                }
                cnt++;
            }
            bufferedReader.close();
            System.out.println("Nodes:" + give_node_get_name.size());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    static void read_GO_CC(String fileName) {
        String line = null;
        StringBuffer buf = new StringBuffer();
        give_annotation_get_genes.clear();
        try {
            int cnt = 0;
            FileReader fileReader = new FileReader(fileName);
            BufferedReader bufferedReader = new BufferedReader(fileReader);
            while ((line = bufferedReader.readLine()) != null) {
                String[] str = line.split("\t");
                if (cnt != 0) {
                    {
                        String[] sss = line.split("\t");
                        String node = sss[1];
                        if (sss.length >= 7) {
                            String go = sss[6];
                            if (give_node_get_name.get(node) != null) {
                                if (go.length() > 0) {
                                    if (!go.contains("|")) {
                                        if (go.startsWith("C:")) {
                                            if (give_annotation_get_genes.get(go) == null) {

                                                ArrayList<String> list = new ArrayList();
                                                list.add(node);
                                                give_annotation_get_genes.put(go, list);
                                            }
                                            if (give_annotation_get_genes.get(go) != null) {
                                                ArrayList<String> list = give_annotation_get_genes.get(go);
                                                list.add(node);
                                            }

                                        }
                                    }
                                    if (go.contains("|")) {

                                        String ss[] = go.split("\\|");
                                        for (int i = 0; i < ss.length; i++) {
                                            String go2 = ss[i];
                                            if (give_annotation_get_genes.get(go2) == null) {
                                                if (go2.startsWith("C:")) {
                                                    ArrayList<String> list = new ArrayList();
                                                    list.add(node);
                                                    give_annotation_get_genes.put(go2, list);
                                                }
                                            }
                                            if (give_annotation_get_genes.get(go2) != null) {
                                                if (go2.startsWith("C:")) {
                                                    ArrayList<String> list = give_annotation_get_genes.get(go2);
                                                    list.add(node);
                                                }
                                            }

                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                cnt++;
            }
            bufferedReader.close();
            ArrayList<String> keys = Collections.list(give_annotation_get_genes.keys());
            System.out.println("keys:" + keys.size());
            for (int i = 0; i < keys.size(); i++) {
                if(give_annotation_get_genes.get(keys.get(i)).size()>=elements_allowed)
                buf.append(keys.get(i).replace("C:", "") + "\t" + give_annotation_get_genes.get(keys.get(i)).toString().replaceAll(" ", "").replace("[", "").replace("]", "") + "\n");

            }
            write_to_file(buf, go_CC_file);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    
    static void read_GO_BP(String fileName) {
        String line = null;
        StringBuffer buf = new StringBuffer();
        give_annotation_get_genes.clear();
        try {
            int cnt = 0;
            FileReader fileReader = new FileReader(fileName);
            BufferedReader bufferedReader = new BufferedReader(fileReader);
            while ((line = bufferedReader.readLine()) != null) {
                String[] str = line.split("\t");
                if (cnt != 0) {
                    {
                        String[] sss = line.split("\t");
                        String node = sss[1];
                        if (sss.length >= 7) {
                            String go = sss[6];
                            if (give_node_get_name.get(node) != null) {
                                if (go.length() > 0) {
                                    if (!go.contains("|")) {
                                        if (go.startsWith("P:")) {
                                            if (give_annotation_get_genes.get(go) == null) {

                                                ArrayList<String> list = new ArrayList();
                                                list.add(node);
                                                give_annotation_get_genes.put(go, list);
                                            }
                                            if (give_annotation_get_genes.get(go) != null) {
                                                ArrayList<String> list = give_annotation_get_genes.get(go);
                                                list.add(node);
                                            }

                                        }
                                    }
                                    if (go.contains("|")) {

                                        String ss[] = go.split("\\|");
                                        for (int i = 0; i < ss.length; i++) {
                                            String go2 = ss[i];
                                            if (give_annotation_get_genes.get(go2) == null) {
                                                if (go2.startsWith("P:")) {
                                                    ArrayList<String> list = new ArrayList();
                                                    list.add(node);
                                                    give_annotation_get_genes.put(go2, list);
                                                }
                                            }
                                            if (give_annotation_get_genes.get(go2) != null) {
                                                if (go2.startsWith("P:")) {
                                                    ArrayList<String> list = give_annotation_get_genes.get(go2);
                                                    list.add(node);
                                                }
                                            }

                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                cnt++;
            }
            bufferedReader.close();
            ArrayList<String> keys = Collections.list(give_annotation_get_genes.keys());
            System.out.println("keys:" + keys.size());
            for (int i = 0; i < keys.size(); i++) {
                if(give_annotation_get_genes.get(keys.get(i)).size()>=elements_allowed)
                buf.append(keys.get(i).replace("P:", "") + "\t" + give_annotation_get_genes.get(keys.get(i)).toString().replaceAll(" ", "").replace("[", "").replace("]", "") + "\n");

            }
            write_to_file(buf, go_BP_file);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    
    
    
    
    static void read_GO_MF(String fileName) {
        String line = null;
        StringBuffer buf = new StringBuffer();
        give_annotation_get_genes.clear();
        try {
            int cnt = 0;
            FileReader fileReader = new FileReader(fileName);
            BufferedReader bufferedReader = new BufferedReader(fileReader);
            while ((line = bufferedReader.readLine()) != null) {
                String[] str = line.split("\t");
                if (cnt != 0) {
                    {
                        String[] sss = line.split("\t");
                        String node = sss[1];
                        if (sss.length >= 7) {
                            String go = sss[6];
                            if (give_node_get_name.get(node) != null) {
                                if (go.length() > 0) {
                                    if (!go.contains("|")) {
                                        if (go.startsWith("F:")) {
                                            if (give_annotation_get_genes.get(go) == null) {

                                                ArrayList<String> list = new ArrayList();
                                                list.add(node);
                                                give_annotation_get_genes.put(go, list);
                                            }
                                            if (give_annotation_get_genes.get(go) != null) {
                                                ArrayList<String> list = give_annotation_get_genes.get(go);
                                                list.add(node);
                                            }

                                        }
                                    }
                                    if (go.contains("|")) {

                                        String ss[] = go.split("\\|");
                                        for (int i = 0; i < ss.length; i++) {
                                            String go2 = ss[i];
                                            if (give_annotation_get_genes.get(go2) == null) {
                                                if (go2.startsWith("F:")) {
                                                    ArrayList<String> list = new ArrayList();
                                                    list.add(node);
                                                    give_annotation_get_genes.put(go2, list);
                                                }
                                            }
                                            if (give_annotation_get_genes.get(go2) != null) {
                                                if (go2.startsWith("F:")) {
                                                    ArrayList<String> list = give_annotation_get_genes.get(go2);
                                                    list.add(node);
                                                }
                                            }

                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                cnt++;
            }
            bufferedReader.close();
            ArrayList<String> keys = Collections.list(give_annotation_get_genes.keys());
            System.out.println("keys:" + keys.size());
            for (int i = 0; i < keys.size(); i++) {
                if(give_annotation_get_genes.get(keys.get(i)).size()>=elements_allowed)
                buf.append(keys.get(i).replace("F:", "") + "\t" + give_annotation_get_genes.get(keys.get(i)).toString().replaceAll(" ", "").replace("[", "").replace("]", "") + "\n");

            }
            write_to_file(buf, go_MF_file);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    
    
    
    
    
    
    
    
    
    static void read_KEGG(String fileName) {
        String line = null;
        StringBuffer buf = new StringBuffer();
        give_annotation_get_genes.clear();
        try {
            int cnt = 0;
            FileReader fileReader = new FileReader(fileName);
            BufferedReader bufferedReader = new BufferedReader(fileReader);
            while ((line = bufferedReader.readLine()) != null) {
                String[] str = line.split("\t");
                if (cnt != 0) {
                    {
                        String[] sss = line.split("\t");
                        String node = sss[1];
                        if (sss.length >= 11) {
                            String kegg = sss[10];
                            if (give_node_get_name.get(node) != null) {
                                if (kegg.length() > 0) {
                                    if (!kegg.contains("|")) {
                                        if (give_annotation_get_genes.get(kegg) == null) {

                                            ArrayList<String> list = new ArrayList();
                                            list.add(node);
                                            give_annotation_get_genes.put(kegg, list);
                                        }
                                        if (give_annotation_get_genes.get(kegg) != null) {
                                            ArrayList<String> list = give_annotation_get_genes.get(kegg);
                                            list.add(node);
                                        }

                                    }
                                    if (kegg.contains("|")) {

                                        String ss[] = kegg.split("\\|");
                                        for (int i = 0; i < ss.length; i++) {
                                            String kegg2 = ss[i];
                                            if (give_annotation_get_genes.get(kegg2) == null) {
                                                ArrayList<String> list = new ArrayList();
                                                list.add(node);
                                                give_annotation_get_genes.put(kegg2, list);
                                            }
                                            if (give_annotation_get_genes.get(kegg2) != null) {
                                                ArrayList<String> list = give_annotation_get_genes.get(kegg2);
                                                list.add(node);
                                            }

                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                cnt++;
            }
            bufferedReader.close();
            ArrayList<String> keys = Collections.list(give_annotation_get_genes.keys());
            System.out.println("keys:" + keys.size());
            for (int i = 0; i < keys.size(); i++) {
                if(give_annotation_get_genes.get(keys.get(i)).size()>=elements_allowed)
                buf.append(keys.get(i) + "\t" + give_annotation_get_genes.get(keys.get(i)).toString().replaceAll(" ", "").replace("[", "").replace("]", "") + "\n");

            }
            write_to_file(buf, kegg_file);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    static void write_to_file(StringBuffer buf, String fileName) throws IOException {
        try {
            BufferedWriter writer = new BufferedWriter(new FileWriter(fileName));
            writer.write(buf.toString());
            writer.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

}

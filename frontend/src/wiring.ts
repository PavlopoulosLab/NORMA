// @ts-nocheck
// ponytail: split verbatim from the former single-file script; typed one file at a time (remove @ts-nocheck as it lands)
import { S } from './state'
import {
  activeView,
  nameForSelection,
  openInNewView,
  renderProfilerNetworkList,
  renderViewBar,
  selectionFromKey,
  uniqueViewName,
} from './profiler'
import { buildModuleNetwork, generateDirectedMultiNetwork, mulberry32 } from './sample_data'
import { convertArena3dNetwork } from './arena3d'
import { cy } from './cy'
import { demoToNormaTexts } from './demo_downloads'
import { drawGroupHulls, resizeHullCanvas } from './hulls'
import { exportNormaAnnotation, exportNormaColors, exportNormaNetwork } from './export_norma'
import { handleNormaUploads } from './uploads'
import { libSelection, plural, setStatus } from './layouts/controls'
import { net3d } from './view3d/state'
import { refreshEnrichmentChoices } from './enrichment'
import { refreshLibraryView } from './library'
import { renderCompareList } from './label_colors'
import { setMode3d } from './view3d/tab'

// Example files from NORMA (github.com/PavlopoulosLab/NORMA, MIT license),
// www/Examples/, embedded verbatim so they work offline.
export const NORMA_EXAMPLE_SETS = {
  'string-tp53': {
    title: 'STRING: TP53 interactors',
    source: 'STRING database export',
    link: null,
    files: [
      {
        kind: 'network',
        name: 'STRING TP53',
        fileName: 'string_interactions.txt',
        text: 'Source\tTarget\tWeight\nCDKN1A\tTP53\t70\nTP53\tMDM2\t2\nMDM4\tTP53\t3\nBCL2L1\tTP53\t4\nCHEK2\tATM\t5\nTP53\tEP300\t20\nATM\tTP53\t7\nTP53\tCREBB\t1\nMDM4\tMDM2\t1\nCHEK2\tTP53\t1\nTP53BP2\tTP53\t1\nCDKN2A\tTP53\t1\nCDKN2A\tMDM2\t1\nATM\tMDM2\t2\nEP300\tCREBBP\t1\nMDM4\tATM\t2\nCHEK2\tMDM4\t1\nCHEK2\tMDM2\t1\nCDKN2A\tCDKN1A\t1\nCDKN2A\tMDM4\t1\nEP300\tMDM2\t1\nCDKN1A\tEP300\t1\nCREBBP\tMDM2\t1\nCDKN1A\tMDM2\t1\nCDKN2A\tBCL2L1\t1\nBCL2L1\tMDM2\t2\nCDKN2A\tATM\t1\nCDKN1A\tMDM4\t1\nCDKN1A\tCHEK2\t1\nCDKN1A\tATM\t2\nCDKN1A\tCREBBP\t1\nCDKN1A\tBCL2L1\t1\nCDKN2A\tCHEK2\t1\nATM\tEP300\t1\nTP53BP2\tBCL2L1\t3\nCDKN2A\tEP300\t3\nBCL2L1\tATM\t3\nMDM4\tEP300\t1\nATM\tCREBBP\t1\nCHEK2\tEP300\t1\nTP53BP2\tMDM2\t1\nCHEK2\tBCL2L1\t1\nCDKN2A\tCREBBP\t1\nTP53BP2\tEP300\t1\nBCL2L1\tEP300\t1\nMDM4\tBCL2L1\t1\n',
      },
      {
        kind: 'annotation',
        name: 'STRING TP53 groups',
        fileName: 'string_interactions_groups_comma_duplicate.txt',
        text: 'Group-1\tBCL2L1,MDM4,MDM2,CHEK2\nGroup-2\tCDKN2A,ATM,TP53BP2,MDM2\nGroup-3\tCHEK2,MDM2\nGroup-4\tTP53,EP300\nGroup-5\tMDM4,MDM2\n',
      },
      {
        kind: 'colors',
        name: 'STRING TP53 expression',
        fileName: 'string_expression_colors.txt',
        text: 'CDKN1A\tblue\nTP53\tblue\nMDM4\tblue\nBCL2L1\tred\nCHEK2\tred\nATM\tred\nTP53BP2\tred\nCDKN2A\tblue\nEP300\tred\nCREBBP\tred\n',
      },
    ],
  },
  'string-bcar3': {
    title: 'STRING: BCAR3 interactors',
    source: 'STRING database export',
    link: null,
    files: [
      {
        kind: 'network',
        name: 'STRING BCAR3',
        fileName: 'BCAR3.txt',
        text: 'Source\tTarget\nVCP\tNPLOC4\nBCAR1\tPXN\nVCP\tDERL2\nNSFL1C\tVCP\nCDC42\tWASL\nVCP\tDERL1\nNPLOC4\tUFD1L\nSYVN1\tVCP\nVCP\tUFD1L\nCDC42\tWAS\nCDC42\tPAK1\nCDC42\tBAIAP2\nVCP\tFAF2\nCDC42\tARHGAP1\nVIMP\tVCP\nCDC42\tPARD6A\nCDC42\tTNK2\nCDC42\tITSN1\nCDC42\tARHGDIA\nCDC42\tPARD6B\nSYVN1\tDERL2\nCDC42\tPAK2\nVIMP\tDERL1\nDERL1\tDERL2\nWAS\tBAIAP2\nCDC42\tPXN\nFAF2\tDERL2\nSYVN1\tDERL1\nUFD1L\tFAF2\nNPLOC4\tFAF2\nBCAR1\tBCAR3\nITSN1\tWASL\nBAIAP2\tWASL\nBCAR1\tTNK2\nBCAR1\tCDC42\nPAK1\tPXN\nPAK1\tARHGDIA\nFAF2\tDERL1\nNEDD9\tPXN\nPAK2\tPAK1\nVIMP\tDERL2\nSYVN1\tFAF2\nVIMP\tSYVN1\nCDC42\tBCAR3\nUFD1L\tDERL1\nPAK2\tPXN\nNSFL1C\tUFD1L\nNEDD9\tBCAR3\nPARD6B\tPARD6A\nBAIAP2\tPXN\nNSFL1C\tNPLOC4\nBCAR1\tBAIAP2\nPAK2\tPARD6A\nSYVN1\tUFD1L\nWAS\tWASL\nTULP4\tSPSB1\nSYVN1\tNPLOC4\nVIMP\tFAF2\nUFD1L\tDERL2\nNPLOC4\tDERL1\nNPLOC4\tDERL2\nITSN1\tWAS\nVIMP\tNPLOC4\nTNK2\tWAS\nNSFL1C\tFAF2\nTULP4\tKCNE4\nBCAR3\tTULP4\nBCAR3\tKCNE4\nNSFL1C\tDERL2\nSAT1\tBCAR3\nNSFL1C\tDERL1\nBCAR3\tVCP\nPAK2\tARHGDIA\nKCNE4\tTULP1\nBCAR3\tTULP1\nPXN\tWASL\nVIMP\tUFD1L\nWAS\tARHGAP1\nWAS\tPXN\nWAS\tPAK1\nBCAR3\tSPSB1\nARHGAP1\tARHGDIA\nBCAR1\tPARD6A\nPAK1\tWASL\nBCAR1\tWASL\nARHGDIA\tPXN\nBCAR1\tWAS\nPAK2\tWASL\nWAS\tPAK2\nARHGAP1\tPXN\nARHGDIA\tWASL',
      },
      {
        kind: 'annotation',
        name: 'BCAR3 KEGG pathways',
        fileName: 'BCAR3_KEGG.txt',
        text: 'Protein processing in endoplasmic reticulum\tDERL1,DERL2,NPLOC4,NSFL1C,SYVN1,UFD1L,VCP,VIMP\nRegulation of actin cytoskeleton\tBAIAP2,BCAR1,CDC42,PAK1,PAK2,PXN,WAS,WASL\nChemokine signaling pathway\tBCAR1,CDC42,PAK1,PXN,WAS,WASL\nBacterial invasion of epithelial cells\tBCAR1,CDC42,PXN,WAS,WASL\nTight junction\tCDC42,PARD6A,PARD6B,WAS,WASL\nAxon guidance\tCDC42,PAK1,PAK2,PARD6A,PARD6B\nFocal adhesion\tBCAR1,CDC42,PAK1,PAK2,PXN\nEndocytosis\tCDC42,PARD6A,PARD6B,WAS,WASL\nAdherens junction\tBAIAP2,CDC42,WAS,WASL\nFc gamma R-mediated phagocytosis\tCDC42,PAK1,WAS,WASL\nRap1 signaling pathway\tBCAR1,CDC42,PARD6A,PARD6B\nHuman papillomavirus infection\tCDC42,PARD6A,PARD6B,PXN\nPathogenic Escherichia coli infection\tCDC42,WAS,WASL\nShigellosis\tCDC42,WAS,WASL\nRenal cell carcinoma\tCDC42,PAK1,PAK2\nSalmonella infection\tCDC42,WAS,WASL\nT cell receptor signaling pathway\tCDC42,PAK1,PAK2\nLeukocyte transendothelial migration\tBCAR1,CDC42,PXN\nProteoglycans in cancer\tCDC42,PAK1,PXN\nRas signaling pathway\tCDC42,PAK1,PAK2\nMAPK signaling pathway\tCDC42,PAK1,PAK2\nVEGF signaling pathway\tCDC42,PXN\nEpithelial cell signaling in Helicobacter pylori infection\tCDC42,PAK1\nErbB signaling pathway\tPAK1,PAK2\nCholine metabolism in cancer\tWAS,WASL\nNeurotrophin signaling pathway\tARHGDIA,CDC42\nHippo signaling pathway\tPARD6A,PARD6B',
      },
      {
        kind: 'annotation',
        name: 'BCAR3 GO molecular function',
        fileName: 'BCAR3_GO_MF.txt',
        text: 'protein binding\tARHGAP1,BAIAP2,BCAR1,BCAR3,CDC42,DERL1,FAF2,ITSN1,KCNE4,NPLOC4,NSFL1C,PAK1,PAK2,PARD6A,PXN,SAT1,SYVN1,TNK2,TULP1,UFD1L,VCP,VIMP,WAS,WASL\nenzyme binding\tARHGAP1,BCAR1,BCAR3,CDC42,DERL1,FAF2,ITSN1,NPLOC4,NSFL1C,PAK1,PAK2,PARD6A,PXN,SYVN1,TNK2,UFD1L,VCP,VIMP,WAS\nmolecular function regulator\tARHGAP1,ARHGDIA,BCAR3,FAF2,ITSN1,NSFL1C,PAK2,TNK2,VCP,WAS,WASL\nenzyme regulator activity\tARHGAP1,ARHGDIA,FAF2,ITSN1,NSFL1C,PAK2,TNK2,VCP,WAS,WASL\nidentical protein binding\tBAIAP2,CDC42,PAK1,PAK2,SAT1,TNK2,VCP,WAS\nprotein kinase binding\tBCAR1,CDC42,PAK1,PAK2,PARD6A,PXN,WAS\nGTPase binding\tARHGAP1,BCAR3,ITSN1,PAK1,PAK2,PARD6A,WAS\nprotein domain specific binding\tARHGAP1,BAIAP2,BCAR1,CDC42,TNK2,VCP,WAS\nubiquitin protein ligase binding\tDERL1,FAF2,NPLOC4,PXN,TNK2,VCP\nRas GTPase binding\tARHGAP1,ITSN1,PAK1,PAK2,PARD6A,WAS\nATPase binding\tDERL1,NSFL1C,SYVN1,UFD1L,VIMP\nRho GTPase binding\tITSN1,PAK1,PAK2,PARD6A,WAS\nGTPase regulator activity\tARHGAP1,ARHGDIA,TNK2,WAS,WASL\nenzyme activator activity\tARHGAP1,ARHGDIA,ITSN1,PAK2,VCP\nubiquitin-specific protease binding\tDERL1,SYVN1,VCP,VIMP\nprotein serine/threonine kinase activity\tCDC42,PAK1,PAK2,TNK2\nRac GTPase binding\tPAK1,PAK2,WAS\nubiquitin binding\tFAF2,NPLOC4,NSFL1C\nSH3 domain binding\tARHGAP1,BCAR1,WAS\nprotein binding, bridging\tARHGAP1,BAIAP2,BCAR3\n',
      },
      {
        kind: 'annotation',
        name: 'BCAR3 GO biological process',
        fileName: 'BCAR3_GO_BP.txt',
        text: 'cellular process\tARHGAP1,ARHGDIA,BAIAP2,BCAR1,BCAR3,CDC42,DERL1,DERL2,FAF2,ITSN1,KCNE4,NEDD9,NPLOC4,NSFL1C,PAK1,PAK2,PARD6A,PARD6B,PXN,SAT1,SPSB1,SYVN1,TNK2,TULP1,TULP4,UFD1L,VCP,VIMP,WAS,WASL\nbiological regulation\tARHGAP1,ARHGDIA,BAIAP2,BCAR1,BCAR3,CDC42,DERL1,DERL2,FAF2,ITSN1,KCNE4,NEDD9,NPLOC4,NSFL1C,PAK1,PAK2,PARD6A,PARD6B,PXN,SAT1,SYVN1,TNK2,TULP1,UFD1L,VCP,VIMP,WAS,WASL\nregulation of biological process\tARHGAP1,ARHGDIA,BAIAP2,BCAR1,BCAR3,CDC42,DERL1,DERL2,ITSN1,KCNE4,NEDD9,NPLOC4,NSFL1C,PAK1,PAK2,PARD6A,PARD6B,PXN,SAT1,SYVN1,TNK2,TULP1,UFD1L,VCP,VIMP,WAS,WASL\nregulation of cellular process\tARHGAP1,ARHGDIA,BAIAP2,BCAR1,BCAR3,CDC42,DERL1,DERL2,ITSN1,NEDD9,NPLOC4,NSFL1C,PAK1,PAK2,PARD6A,PARD6B,PXN,SAT1,SYVN1,TNK2,TULP1,UFD1L,VCP,VIMP,WAS,WASL\nresponse to stimulus\tARHGAP1,ARHGDIA,BAIAP2,BCAR1,BCAR3,CDC42,DERL1,DERL2,FAF2,ITSN1,NEDD9,NPLOC4,PAK1,PAK2,PARD6A,PXN,SYVN1,TNK2,TULP1,UFD1L,VCP,VIMP,WAS,WASL\nlocalization\tARHGAP1,BAIAP2,BCAR1,CDC42,DERL1,DERL2,FAF2,ITSN1,KCNE4,NPLOC4,NSFL1C,PAK1,PAK2,PXN,SYVN1,TNK2,TULP1,TULP4,UFD1L,VCP,VIMP,WAS,WASL\ncellular response to stimulus\tARHGAP1,ARHGDIA,BAIAP2,BCAR1,BCAR3,CDC42,DERL1,DERL2,FAF2,ITSN1,NEDD9,NPLOC4,PAK1,PAK2,PARD6A,PXN,SYVN1,TNK2,UFD1L,VCP,VIMP,WAS,WASL\nsignal transduction\tARHGAP1,ARHGDIA,BAIAP2,BCAR1,BCAR3,CDC42,DERL1,DERL2,ITSN1,NEDD9,PAK1,PAK2,PARD6A,PXN,SYVN1,TNK2,VCP,VIMP,WAS,WASL\nestablishment of localization\tARHGAP1,BAIAP2,CDC42,DERL1,DERL2,FAF2,ITSN1,KCNE4,NPLOC4,NSFL1C,PAK1,SYVN1,TNK2,TULP1,UFD1L,VCP,VIMP,WAS,WASL\npositive regulation of biological process\tARHGAP1,ARHGDIA,BAIAP2,BCAR1,BCAR3,CDC42,DERL1,DERL2,ITSN1,NSFL1C,PAK1,PAK2,PARD6A,PXN,TNK2,TULP1,VCP,WAS,WASL\nregulation of response to stimulus\tARHGAP1,ARHGDIA,BAIAP2,BCAR1,BCAR3,CDC42,DERL2,ITSN1,NPLOC4,PAK1,PAK2,PXN,SYVN1,UFD1L,VCP,VIMP,WAS,WASL\ntransport\tARHGAP1,BAIAP2,CDC42,DERL1,DERL2,FAF2,ITSN1,KCNE4,NPLOC4,PAK1,SYVN1,TNK2,TULP1,UFD1L,VCP,VIMP,WAS,WASL\npositive regulation of cellular process\tARHGAP1,ARHGDIA,BAIAP2,BCAR1,BCAR3,CDC42,DERL1,DERL2,ITSN1,NSFL1C,PAK1,PAK2,PXN,TNK2,TULP1,VCP,WAS,WASL\ncellular component organization\tBAIAP2,BCAR1,CDC42,DERL1,FAF2,ITSN1,NEDD9,NPLOC4,NSFL1C,PAK1,PAK2,PARD6A,PARD6B,PXN,TULP1,VCP,WAS,WASL\nresponse to chemical\tBAIAP2,BCAR1,BCAR3,CDC42,DERL1,DERL2,FAF2,NPLOC4,PAK1,PAK2,PARD6A,PXN,SYVN1,UFD1L,VCP,VIMP,WAS\ncellular localization\tARHGAP1,CDC42,DERL1,DERL2,FAF2,ITSN1,NPLOC4,NSFL1C,SYVN1,TULP1,TULP4,UFD1L,VCP,VIMP,WAS,WASL\nresponse to organic substance\tBAIAP2,BCAR1,CDC42,DERL1,DERL2,FAF2,NPLOC4,PAK1,PAK2,PARD6A,PXN,SYVN1,UFD1L,VCP,VIMP,WAS\nnegative regulation of biological process\tARHGAP1,ARHGDIA,CDC42,DERL2,ITSN1,NPLOC4,NSFL1C,PAK1,PAK2,PARD6A,SYVN1,TNK2,UFD1L,VIMP,WAS,WASL\norganonitrogen compound metabolic process\tCDC42,DERL1,DERL2,FAF2,NPLOC4,NSFL1C,PAK1,PAK2,SAT1,SPSB1,SYVN1,TNK2,TULP4,UFD1L,VCP,VIMP\ncellular protein metabolic process\tCDC42,DERL1,DERL2,FAF2,NPLOC4,NSFL1C,PAK1,PAK2,SPSB1,SYVN1,TNK2,TULP4,UFD1L,VCP,VIMP\nestablishment of localization in cell\tARHGAP1,CDC42,DERL1,DERL2,FAF2,ITSN1,NPLOC4,NSFL1C,SYVN1,UFD1L,VCP,VIMP,WAS,WASL\ncellular response to organic substance\tBAIAP2,BCAR1,CDC42,DERL1,DERL2,PAK1,PAK2,PARD6A,PXN,SYVN1,UFD1L,VCP,VIMP,WAS\nregulation of signal transduction\tARHGAP1,ARHGDIA,BAIAP2,BCAR3,CDC42,ITSN1,NPLOC4,PAK1,PAK2,PXN,SYVN1,UFD1L,VCP,VIMP\nresponse to stress\tCDC42,DERL1,DERL2,FAF2,NPLOC4,PAK1,PAK2,PXN,SYVN1,TNK2,UFD1L,VCP,VIMP,WAS\nnegative regulation of cellular process\tARHGAP1,ARHGDIA,CDC42,DERL2,ITSN1,NPLOC4,PAK1,PAK2,PARD6A,SYVN1,UFD1L,VIMP,WAS,WASL\nprotein localization\tARHGAP1,BAIAP2,DERL1,DERL2,FAF2,NPLOC4,SYVN1,TULP1,TULP4,UFD1L,VCP,VIMP,WASL\ncell surface receptor signaling pathway\tARHGDIA,BAIAP2,BCAR1,CDC42,ITSN1,NEDD9,PAK1,PAK2,PARD6A,PXN,TNK2,WAS,WASL\nregulation of cellular component organization\tARHGDIA,BAIAP2,BCAR1,CDC42,DERL2,NSFL1C,PAK1,PXN,TNK2,TULP1,VCP,WAS,WASL\nregulation of localization\tARHGAP1,BCAR1,DERL2,KCNE4,NSFL1C,PAK1,PARD6A,PARD6B,TNK2,TULP1,VIMP,WAS,WASL\nregulation of molecular function\tARHGAP1,ARHGDIA,BCAR3,DERL1,FAF2,ITSN1,NSFL1C,PAK1,PAK2,TNK2,VCP,WAS,WASL\nresponse to organonitrogen compound\tBAIAP2,BCAR1,DERL1,DERL2,FAF2,NPLOC4,PAK1,PXN,SYVN1,UFD1L,VCP,VIMP\nintracellular transport\tARHGAP1,CDC42,DERL1,DERL2,FAF2,NPLOC4,SYVN1,UFD1L,VCP,VIMP,WAS,WASL\ncellular protein modification process\tCDC42,DERL1,DERL2,PAK1,PAK2,SPSB1,SYVN1,TNK2,TULP4,UFD1L,VCP,VIMP\norganelle organization\tBAIAP2,BCAR1,CDC42,FAF2,NEDD9,NPLOC4,NSFL1C,PAK1,PAK2,PARD6A,WAS,WASL\ncellular protein localization\tDERL1,DERL2,FAF2,NPLOC4,SYVN1,TULP1,TULP4,UFD1L,VCP,VIMP,WASL\nvesicle-mediated transport\tARHGAP1,BAIAP2,CDC42,FAF2,ITSN1,PAK1,TNK2,TULP1,VCP,WAS,WASL\npositive regulation of response to stimulus\tARHGAP1,BAIAP2,BCAR1,BCAR3,CDC42,ITSN1,PAK1,PAK2,VCP,WAS,WASL\nregulation of catalytic activity\tARHGAP1,ARHGDIA,FAF2,ITSN1,NSFL1C,PAK1,PAK2,TNK2,VCP,WAS,WASL\ncellular component assembly\tBAIAP2,CDC42,DERL1,NEDD9,NSFL1C,PARD6A,PARD6B,PXN,VCP,WAS,WASL\nenzyme linked receptor protein signaling pathway\tBAIAP2,BCAR1,CDC42,ITSN1,PAK1,PAK2,PARD6A,PXN,TNK2,WASL\nregulation of immune response\tBAIAP2,BCAR1,CDC42,NPLOC4,PAK1,PAK2,UFD1L,VIMP,WAS,WASL\ncytoskeleton organization\tBAIAP2,BCAR1,CDC42,NEDD9,NSFL1C,PAK1,PAK2,PARD6A,WAS,WASL\norganonitrogen compound catabolic process\tDERL1,DERL2,FAF2,NPLOC4,NSFL1C,SAT1,SYVN1,UFD1L,VCP,VIMP\npositive regulation of cellular component organization\tARHGDIA,BAIAP2,CDC42,NSFL1C,PAK1,PXN,TULP1,VCP,WAS,WASL\ncellular response to stress\tDERL1,DERL2,FAF2,NPLOC4,PAK2,PXN,SYVN1,UFD1L,VCP,VIMP\ncellular catabolic process\tDERL1,DERL2,FAF2,NPLOC4,NSFL1C,SAT1,SYVN1,UFD1L,VCP,VIMP\nregulation of intracellular signal transduction\tARHGAP1,ARHGDIA,CDC42,ITSN1,NPLOC4,PAK1,PAK2,SYVN1,UFD1L,VIMP\nmulti-organism process\tBAIAP2,CDC42,DERL1,DERL2,ITSN1,PAK2,PARD6A,VCP,VIMP,WASL\nimmune system process\tBAIAP2,BCAR1,CDC42,FAF2,PAK1,PAK2,TNK2,VCP,WAS,WASL\nproteasome-mediated ubiquitin-dependent protein catabolic process\tDERL1,DERL2,FAF2,NPLOC4,NSFL1C,SYVN1,UFD1L,VCP,VIMP\ntransmembrane receptor protein tyrosine kinase signaling pathway\tBAIAP2,BCAR1,CDC42,ITSN1,PAK1,PAK2,PXN,TNK2,WASL\nimport into cell\tARHGAP1,BAIAP2,CDC42,ITSN1,PAK1,TNK2,TULP1,WAS,WASL\nprotein transport\tARHGAP1,DERL1,DERL2,FAF2,NPLOC4,SYVN1,UFD1L,VCP,VIMP\nregulation of cellular protein metabolic process\tBCAR3,DERL1,DERL2,NSFL1C,PAK1,PAK2,PARD6A,TNK2,VCP\nretrograde protein transport, ER to cytosol\tDERL1,DERL2,FAF2,NPLOC4,SYVN1,UFD1L,VCP,VIMP\nsmall GTPase mediated signal transduction\tARHGAP1,BCAR3,CDC42,ITSN1,PAK1,PAK2,TNK2,WAS\nactin cytoskeleton organization\tBAIAP2,BCAR1,CDC42,NEDD9,PAK1,PAK2,WAS,WASL\nregulation of cytoskeleton organization\tARHGDIA,BAIAP2,CDC42,NSFL1C,PAK1,PXN,WAS,WASL\nendocytosis\tBAIAP2,CDC42,ITSN1,PAK1,TNK2,TULP1,WAS,WASL\nprotein modification by small protein conjugation or removal\tCDC42,DERL1,SPSB1,SYVN1,TULP4,UFD1L,VCP,VIMP\ngeneration of neurons\tARHGDIA,BAIAP2,CDC42,PAK1,PAK2,PARD6B,TULP1,WASL\nregulation of apoptotic process\tARHGDIA,BCAR1,ITSN1,PAK1,PAK2,SYVN1,VCP,VIMP\nregulation of phosphate metabolic process\tBCAR3,ITSN1,NSFL1C,PAK1,PAK2,PARD6A,TNK2,VCP\nregulation of transport\tARHGAP1,DERL2,KCNE4,PAK1,TNK2,TULP1,VIMP,WASL\nregulation of protein modification process\tBCAR3,DERL1,NSFL1C,PAK1,PAK2,PARD6A,TNK2,VCP\nanatomical structure morphogenesis\tBAIAP2,BCAR3,CDC42,PAK1,PARD6B,SAT1,TULP1,WASL\nubiquitin-dependent ERAD pathway\tDERL1,DERL2,FAF2,NPLOC4,SYVN1,VCP,VIMP\nresponse to topologically incorrect protein\tDERL1,DERL2,FAF2,SYVN1,UFD1L,VCP,VIMP\npositive regulation of cytoskeleton organization\tBAIAP2,CDC42,NSFL1C,PAK1,PXN,WAS,WASL\nimmune response-activating cell surface receptor signaling pathway\tBAIAP2,BCAR1,CDC42,PAK1,PAK2,WAS,WASL\nregulation of actin cytoskeleton organization\tARHGDIA,BAIAP2,CDC42,PAK1,PXN,WAS,WASL\npositive regulation of cellular component biogenesis\tBAIAP2,CDC42,PAK1,PXN,VCP,WAS,WASL\nneuron projection development\tBAIAP2,CDC42,PAK1,PAK2,PARD6B,TULP1,WASL\nprotein ubiquitination\tCDC42,DERL1,SPSB1,SYVN1,TULP4,VCP,VIMP\nimmune effector process\tBAIAP2,CDC42,FAF2,PAK1,VCP,WAS,WASL\nregulation of response to stress\tDERL2,NPLOC4,PAK1,PAK2,SYVN1,UFD1L,VIMP\nmovement of cell or subcellular component\tBCAR1,CDC42,PAK2,PXN,TNK2,WAS,WASL\nregulation of phosphorylation\tBCAR3,ITSN1,PAK1,PAK2,PARD6A,TNK2,VCP\npositive regulation of signal transduction\tARHGAP1,BAIAP2,BCAR3,ITSN1,PAK1,PAK2,VCP\nFc receptor signaling pathway\tBAIAP2,CDC42,PAK1,PAK2,WAS,WASL\ncellular response to topologically incorrect protein\tDERL1,DERL2,SYVN1,UFD1L,VCP,VIMP\nresponse to unfolded protein\tDERL1,DERL2,FAF2,SYVN1,VCP,VIMP\npositive regulation of supramolecular fiber organization\tBAIAP2,CDC42,PAK1,PXN,WAS,WASL\nphagocytosis\tBAIAP2,CDC42,PAK1,TULP1,WAS,WASL\nactin filament organization\tBAIAP2,BCAR1,CDC42,NEDD9,WAS,WASL\nregulation of actin filament organization\tBAIAP2,CDC42,PAK1,PXN,WAS,WASL\nregulation of protein complex assembly\tBAIAP2,CDC42,PAK1,VCP,WAS,WASL\nviral process\tCDC42,DERL1,ITSN1,PAK2,PARD6A,VCP\nregulation of plasma membrane bounded cell projection organization\tARHGDIA,BAIAP2,CDC42,PAK1,WAS,WASL\nregulation of growth\tBCAR1,CDC42,DERL2,NEDD9,PAK1,PAK2\npositive regulation of phosphorylation\tBCAR3,ITSN1,PAK1,PAK2,TNK2,VCP\ncellular response to endogenous stimulus\tBAIAP2,BCAR1,PAK1,PARD6A,PXN,VIMP\npositive regulation of protein modification process\tBCAR3,DERL1,PAK1,PAK2,TNK2,VCP\nregulation of hydrolase activity\tARHGAP1,ARHGDIA,NSFL1C,PAK2,TNK2,VCP\nvascular endothelial growth factor receptor signaling pathway\tBAIAP2,BCAR1,CDC42,PAK2,PXN\nFc-gamma receptor signaling pathway involved in phagocytosis\tBAIAP2,CDC42,PAK1,WAS,WASL\ndendrite development\tBAIAP2,CDC42,PAK2,TULP1,WASL\nendoplasmic reticulum unfolded protein response\tDERL1,DERL2,SYVN1,VCP,VIMP\nestablishment or maintenance of cell polarity\tCDC42,NSFL1C,PAK1,PARD6A,PARD6B\n',
      },
    ],
  },
  tau: {
    title: 'Drosophila Tau network',
    source: 'PMID 31488613, PMCID PMC6794924',
    link: 'https://doi.org/10.1523/JNEUROSCI.0391-19.2019',
    files: [
      {
        kind: 'network',
        name: 'Drosophila Tau',
        fileName: 'TAU_network_DEGs_NORMA.txt',
        text: "Source\tTarget\nCG6178\tyip2\nshep\tScpX\nnito\tRef1\nbol\tHrb27C\nLar\tAbl\nLar\tCadN\nCG31715\tcpb\nMapmodulin\tAats-ile\nCG5590\tCG17597\nSc2\tOst48\nfrj\tCG10932\nKap-alpha3\temb\nUch\tUsp5\nPrx2540-1\tPrx2540-2\nCG2082\tCG2091\nlig\tCapr\nlig\trin\nclu\tDp1\nclu\teIF3-S10\nclu\trin\nclu\teIF3-S8\nclu\teIF3-S9\nCG9090\tCyt-c-p\nCG9090\tCOX5A\nAce\tCat\nDbp80\temb\nSsadh\tCG33791\nSsadh\tCG32026\nSsadh\tMdh2\nCG4572\tCG10576\nCG4572\tHsc70-3\nPlexA\talpha-Cat\nl(1)G0156\tCG11876\nl(1)G0156\tSsadh\nl(1)G0156\tCG33791\nPlc21C\tGbeta76C\nPlc21C\tPkc53E\nHrb27C\tpoe\nHrb27C\theph\nHrb27C\tPyK\nHrb27C\tbel\nHrb27C\tme31B\nHrb27C\tpAbp\nHrb27C\tTER94\nKhc\tTAU\nCG32026\tCG33791\nbel\tRef1\nbel\tpoe\nbel\temb\nbel\tvig\nbel\tme31B\nDp1\tCG2918\nSgt\tTrap1\nSgt\tTcp-1zeta\nMen-b\tCG11876\nMen-b\tPyK\nMen-b\tMdh2\nCG11899\tCG3011\nMgstl\tCG3529\nMgstl\tOst48\nCG14715\tCG2852\nLpin\tPkc53E\ngammaSnap2\tzetaCOP\ngammaSnap2\tbetaCOP\ngammaSnap2\tepsilonCOP\ngammaSnap2\tbeta'COP\ngammaSnap2\tdeltaCOP\nCG10359\tGp93\nCG10359\tCG1354\nCG10359\tCaBP1\nCG10359\tPdi\nKP78a\tCG32683\nKP78a\tArr2\nCG9257\tpoe\nCG9257\tCG3529\nEct3\tRpn5\nEct3\tCG6891\nEct3\tCat\nEct3\tCalpB\nCG4882\tbonsai\nCalpB\tCG6891\nCalpB\tCat\npar-1\tCG32683\npar-1\tKP78a\npar-1\tArr2\nPrx5\tPrx2540-2\nPrx5\tCat\nPkc53E\tzip\nCG31064\tRab5\nNlp\tmago\nNlp\teEF1delta\nNlp\tsmt3\nCG10186\tbetaCOP\nCG10186\tcpb\nCG10186\tcpa\nCG10186\tbeta'COP\nCG10186\tbai\nCG10186\tepsilonCOP\nCG10186\tDlic\nCG10186\tp115\nCG10186\tzetaCOP\nCG10186\tdeltaCOP\nrin\tCapr\nLpR1\tArr2\nLpR1\tArr1\nLpR1\tUbqn\nLpR1\tCG32683\nLpR1\tlqf\nLpR1\tHrs\nLpR1\tLpR2\nLpR1\tRab5\nCG16935\tCG12262\nCG16935\tyip2\npoe\tbetaCOP\npoe\tCG3529\nCG43367\tbetaCOP\nCG43367\tpoe\nRab39\tRab5\nRab39\tGdi\nemb\tRae1\nemb\talphaCOP\nemb\tCG10576\nAGBE\tEno\nAGBE\tPyK\nLpR2\tUbqn\nLpR2\tCG32683\nLpR2\tArr2\nLpR2\tHrs\nLpR2\tlqf\nLpR2\tRab5\nCG3529\tOst48\nOst48\tCG2918\nHsp60C\tHop\nHsp60C\tHsc70-3\nPrx2540-2\tCat\nEfTuM\tRpS14a\nEfTuM\tHsc70-5\nEfTuM\tRpL10Ab\nEfTuM\tRpS13\nEfTuM\tRpS16\nEfTuM\tRpS23\nEfTuM\tRpS15Aa\nEfTuM\tRpL4\nEfTuM\tCG11876\nEfTuM\tbonsai\nEfTuM\tmRpS10\nEfTuM\tRpL12\nEfTuM\tsta\nEfTuM\tRpL3\nEfTuM\tHsp60\nEfTuM\tRpS18\nEfTuM\tRpS2\nEfTuM\tmRpS9\nEfTuM\tRpS3\nEfTuM\tmRpL12\nEfTuM\tRpL11\nfne\tTango4\nfne\tSrp54\nfne\tCG10777\nfne\tPrp8\nfne\tmago\nfne\tCG16941\nfne\typs\nfne\tCG10077\nfne\tRbp9\nfne\tU2A\nfne\tPkc53E\nfne\tHrb98DE\nfne\tLS2\nfne\tSF2\nfne\tmub\nfne\theph\nKlp10A\tdeltaCOP\nMtl\tzip\nMtl\ttsr\nMtl\tRhoGDI\nHsp60\tCG7033\nRanBPM\tCG6617\nyps\tTango4\nyps\tLS2\nyps\tmago\nyps\tCG10777\nArt4\tCG10777\nArt4\tCG10077\nArt4\tpAbp\nImpL3\tCat\nImpL3\tMen-b\nImpL3\tPfk\nImpL3\tMen\nImpL3\tEno\nImpL3\tTpi\nImpL3\tPyK\nImpL3\tCG11876\nCG10077\tCG10777\nCG10077\tLS2\nCG10077\tTango4\nCG10077\typs\nCG10077\tmago\nLam\tRab5\nLamC\tLam\nUbqn\tHDAC6\nUbqn\tCG32683\nmub\tCG3800\nmub\tmago\nmub\tU2A\nmub\tSrp54\nmub\tTango4\nmub\tPrp8\nmub\tCG10777\nmub\tCG16941\nmub\typs\nmub\tCG10077\nmub\tLS2\nmub\tHrb98DE\nmub\tRbp9\nmub\tSF2\nAGO1\tRbp9\nAGO1\tpAbp\nAGO1\tRae1\nAGO1\tme31B\nglo\tHrb98DE\nSap-r\tCG3529\nSap-r\tMgstl\nSap-r\tpins\nSap-r\tRab5\nSap-r\tOst48\nNdg\tCg25C\nNdg\tLanB2\ntrol\tCp1\ntrol\tLanB2\ntrol\tCg25C\ntrol\tNdg\nHsp70Ab\tPdi\nHsp70Ab\tDnaJ-H\nHsp70Ab\tHop\nHsp70Ab\tHsp27\nbai\tSrp19\nbai\tCG2852\nbai\tgammaSnap2\nbai\tKlp10A\nbai\tKlc\nbai\tDlic\nbai\tcpa\nbai\tp115\nbai\tcpb\nbai\tdeltaCOP\nbai\tbetaCOP\nbai\tepsilonCOP\nbai\tbeta'COP\nbai\tzetaCOP\nNc73EF\tSsadh\nNc73EF\tCG32026\nNc73EF\tl(1)G0156\nNc73EF\tCG33791\nNc73EF\tCG11876\nSrp54\typs\nSrp54\tCG10777\nSrp54\tmago\nSrp54\tLS2\nSrp54\tCG10077\nSrp54\tTango4\nUgt\tCG2918\nRbp9\tCG10777\nRbp9\tTango4\nRbp9\tSrp54\nRbp9\tPrp8\nRbp9\tCG10077\nRbp9\tCG16941\nRbp9\tU2A\nRbp9\tPkc53E\nRbp9\tLS2\nRbp9\tHrb98DE\nRbp9\typs\nRbp9\tmago\nAct79B\tcpb\nAct79B\tflr\nAct79B\tzip\nRbp2\tpAbp\nRbp2\tRpL10Ab\nRbp2\tRpS4\nRbp2\tRpL18A\nRbp2\tRpS3\nRbp2\tRpS11\nRbp2\teIF3-S9\nRbp2\tRpL11\nRbp2\teIF3-S10\nRbp2\tRpL13\nRbp2\teIF3-S8\nRbp2\tRpS10b\nRbp2\tRpS10a\nRbp2\tRpS27\nRbp2\tRpL3\nRbp2\tRpS12\nRbp2\tRpS7\nRbp2\tRpL12\nRbp2\tTango7\nRbp2\tRpS23\nRbp2\tRpS13\nRbp2\tsta\nRbp2\tRpS18\nRbp2\tRpS16\nRbp2\tRpL4\nRbp2\tRpS6\nRbp2\tTrip1\nRbp2\tRpS2\nRbp2\teIF-3p66\nRbp2\tCG5642\nRbp2\tCG9769\nIswi\tHis1\nIswi\tBap55\nIswi\tsmt3\nIswi\tmor\nVps60\tHrs\neEF1delta\teIF3-S10\neEF1delta\tRpS10b\neEF1delta\tRpS13\neEF1delta\tRpS6\neEF1delta\tRpS16\neEF1delta\tRpL12\neEF1delta\tsta\neEF1delta\tRpS2\neEF1delta\tCG5642\neEF1delta\tRpL13\neEF1delta\tTrip1\nCG10576\tRpS6\nCG10576\tRpS4\nCG10576\tCapr\nCG10576\tsta\nCG10576\tRpS16\nCG10576\tTcp-1zeta\nCG10576\twuho\nCG10576\tRpL12\nCG10576\tCG7033\nAbl\tDscam1\nAbl\tsyd\nAbl\talpha-Cat\nAbl\tarm\nRae1\tmago\nMtor\tmago\nMtor\tAGO1\nMtor\tHsc70-3\nMtor\tHsc70-5\nMtor\tsmt3\nMtor\tRae1\nRef1\tLS2\nRef1\tmago\nTrap1\tHsp70Ab\nTrap1\tHop\nTrap1\tHsp60C\nTrap1\tHsp60\nTrap1\tCG2918\nTrap1\tHsc70-3\nTrap1\tCG1416\nCG17597\tCat\nCG17597\tScpX\nCG17597\tMfe2\nlqf\tCG32683\nlqf\tUbqn\nlqf\tArr2\nlqf\tRab5\nlqf\tHrs\nTim17b\tTim9a\nCG3902\tCG10932\nCat\tCG6891\np115\tdeltaCOP\nSec24AB\tdeltaCOP\nSec24AB\tbetaCOP\nSec24AB\tepsilonCOP\nSec24AB\tzetaCOP\nSec24AB\talphaCOP\nSec24AB\tbeta'COP\nSec24AB\tp115\nninaC\tAct79B\nninaC\tzip\nninaC\tArr2\nmago\tsta\nmago\tRpS6\nmago\tCG10777\nCG11876\tMen\nCG11876\tMdh2\nCG11876\tPyK\nCG11876\tND-PDSW\nCG11876\tAcCoAS\nCG11876\tEno\nCG11876\tCG33791\nPdk\tade3\nPdk\tCG11876\nB52\tRpS3\nB52\tHrb27C\nB52\tglo\nB52\tCG10777\nB52\tfne\nB52\tCG10077\nB52\typs\nB52\tRbp9\nB52\tCG16941\nB52\tTango4\nB52\tU2A\nB52\tmago\nB52\tSrp54\nB52\tmub\nB52\theph\nB52\tPrp8\nB52\tLS2\nB52\tHrb98DE\nB52\tU2af50\nB52\tSF2\nVha100-1\tsun\nVha100-1\tVha100-2\nVha100-1\tVha68-2\nrobl\tDlic\nHsc70-5\tCG7033\nHsc70-5\tT-cp1\nHsc70-5\tTpi\nHsc70-5\tDnaJ-H\nHsc70-5\tTcp-1eta\nHsc70-5\tHop\nHsc70-5\tRae1\nHsc70-5\tTrap1\nHsc70-5\tHsp60C\nHsc70-5\tHsc70-3\nHsc70-5\tHsp60\nHsc70-5\tGp93\ntsr\tsqh\ntsr\tcpa\ntsr\tRpS6\nflr\tcpa\nflr\ttsr\nAcCoAS\tCG12262\nAcCoAS\tCG10932\nACC\tEno\nACC\tCG10932\nACC\tAcCoAS\nLS2\tmago\nLS2\tCG10777\nFKBP59\tninaC\nFKBP59\tDlic\nFKBP59\tcpb\nFKBP59\tcpa\nFKBP59\tHop\nU2af50\tRef1\nU2af50\tLS2\nU2af50\typs\nU2af50\tCG10777\nU2af50\tCG10077\nU2af50\tRbp9\nU2af50\tfne\nU2af50\tSrp54\nU2af50\tmago\nU2af50\tmub\nU2af50\tTango4\nU2af50\theph\nU2af50\tCG16941\nU2af50\tHrb98DE\nU2af50\tPrp8\nU2af50\tSF2\nU2af50\tU2A\nFmr1\tdock\nFmr1\tPyK\nFmr1\tpoe\nFmr1\tbel\nFmr1\tpAbp\nFmr1\teIF4G\nFmr1\tDscam1\nFmr1\tRanBPM\nFmr1\tHrb98DE\nFmr1\tHrb27C\nFmr1\tCapr\nFmr1\tTER94\nFmr1\tRbp9\nFmr1\tme31B\nFmr1\tAGO1\nFmr1\tvig\nScpX\tCat\nScpX\tMfe2\nTop2\tRpS13\nTop2\tPkc53E\nTop2\tUba2\nTop2\tIswi\nTop2\tsmt3\nCG8209\tUfd1-like\nheph\tTango4\nheph\tSrp54\nheph\tCG16941\nheph\tCG10077\nheph\tCG10777\nheph\tU2A\nheph\tmago\nheph\typs\nheph\tPrp8\nheph\tmub\nheph\tLS2\nheph\tpAbp\nheph\tHrb98DE\nheph\tRbp9\nheph\tSF2\nHrs\tCG4572\nHrs\tCG32683\nHrs\tArr2\nHrs\tUbqn\nHrs\tRab5\nHrb98DE\tCG10777\nHrb98DE\tmago\nHrb98DE\tSrp54\nHrb98DE\tCG16941\nHrb98DE\tCG10077\nHrb98DE\typs\nHrb98DE\tLS2\nHrb98DE\tTango4\nbic\tRpS3\nNacalpha\tRpS13\nNacalpha\tRpS2\nNacalpha\tsta\nNacalpha\tRpS6\nNacalpha\tRpS16\nNacalpha\tRpS27\nNacalpha\tRpS4\nNacalpha\tRpS23\nNacalpha\tRpS18\nNacalpha\tRpL12\nNacalpha\tRpL18A\nNacalpha\tRpL11\nNacalpha\tRpS10b\nNacalpha\tRpS17\nNacalpha\tRpL13\nNacalpha\tRpS3\nNacalpha\tRpL4\nNacalpha\tbic\nAats-val\teEF1delta\nAats-val\tEf1beta\nAats-val\tAats-asp\nAats-val\tAats-ile\nAats-val\tCG33123\nAats-val\tAats-arg\nBicD\tLam\nBicD\tbic\nBicD\tKhc\nBicD\tcpb\nBicD\tcpa\nBicD\tDlic\nDnaJ-H\tCG2918\nDnaJ-H\tHsc70-3\nDnaJ-H\tHop\nmRpS34\tCG4882\nmRpS34\tbonsai\nCadN\talpha-Cat\nCadN\tarm\nvig\tRpS15Aa\nvig\tRpS13\nvig\tRpS16\nvig\tRpS17\nvig\tRpL12\nTudor-SN\tbel\nTudor-SN\tAGO1\nTudor-SN\tFmr1\nTudor-SN\tvig\nMdh2\tAldh\nMdh2\tND-51\nMdh2\tCOX4\nMdh2\tCyt-c-p\nMdh2\tCOX5A\nMdh2\tMen\nMdh2\tEno\nCyt-c-p\tCat\nCyt-c-p\tporin\nCyt-c-p\tND-PDSW\nCyt-c-p\tCOX6B\nAldh\tCat\nAldh\tEno\nAldh\tCG12262\nAldh\tAcCoAS\nCG15093\tCG12262\nCG15093\tAcCoAS\nCG15093\tCG3902\nCG15093\tAldh\nCG4679\tmRpS34\nCG4679\tbsf\nCG4679\tCG4882\nCG4679\tmRpL12\nCG4679\tmRpL44\nCG4679\tbonsai\nCG4679\tmRpS10\nCG4679\tmRpS23\nCG4679\tmRpS22\nCG4679\tmRpS9\nCG4679\tmRpS18B\nSF2\tCG10777\nSF2\tCG10077\nSF2\tU2A\nSF2\tTango4\nSF2\typs\nSF2\tPrp8\nSF2\tRbp9\nSF2\tCG16941\nSF2\tSrp54\nSF2\tRef1\nSF2\tLS2\nSF2\tmago\nSF2\tHrb98DE\np47\tCG8209\np47\tUfd1-like\np47\tRpt2\nRad23\tme31B\nRad23\tUch-L5\nRad23\tRpt1\nRad23\tHDAC6\nRad23\tUsp5\nRad23\tUba2\nRad23\tUfd1-like\nRad23\tSgt\nRad23\tRpt5\nRad23\tBap55\nRad23\tpont\nRad23\tCG8209\nRad23\tIswi\nRad23\tRpn13\nRad23\tUbqn\nRad23\tsmt3\nRad23\tRpt2\nRad23\tTER94\nme31B\tpoe\nme31B\tCG2091\nme31B\typs\nND-51L1\tND-51\nND-51L1\tNP15.6\nND-51L1\tND-PDSW\nND-51L1\tND-B16.6\nsyd\talpha-Cat\nsyd\tDlic\nsyd\tKlc\nArr2\tUbqn\nArr2\tCG32683\nArr1\tninaC\nArr1\tchp\nArr1\tKP78a\nArr1\tLpR2\nArr1\tUbqn\nArr1\tCG32683\nArr1\tpar-1\nArr1\tHrs\nArr1\tlqf\nArr1\tRab5\nArr1\tArr2\nND-51\tND-B16.6\nND-51\tND-PDSW\nCG16941\typs\nCG16941\tmago\nCG16941\tCG10777\nCG16941\tCG10077\nCG16941\tSrp54\nCG16941\tLS2\nCG16941\tTango4\nCnx99A\tOst48\nCnx99A\tHsp70Ab\nCnx99A\tCG2918\nCnx99A\tUgt\nCnx99A\tGp93\nCnx99A\tPdi\nCnx99A\tHsc70-3\nCnx99A\tERp60\nAats-his\tAats-asp\nAats-his\tCct5\nAats-his\tAats-arg\nAats-his\tAats-val\nAats-his\tCG33123\nAats-his\tAats-ile\nmRpS35\tRpS2\nmRpS35\tCG4882\nmRpS35\tmRpL12\nmRpS35\tmRpS34\nmRpS35\tmRpL44\nmRpS35\tCG4679\nmRpS35\tmRpS23\nmRpS35\tmRpS18B\nmRpS35\tmRpS10\nmRpS35\tmRpS9\nmRpS35\tmRpS22\nmRpS35\tbonsai\nmRpS35\tmRpS5\nVha68-2\tsun\nVha68-2\tEno\nVha100-2\tsun\nVha100-2\tVha68-2\nRac1\tsqh\nRac1\tFmr1\nRac1\ttsr\nRac1\tDscam2\nRac1\tGdi\nRac1\tAbl\nRac1\tdock\nRac1\tzip\nRac1\tRhoGDI\nmRpS7\tade3\nmRpS7\tCG9769\nmRpS7\tCG4882\nmRpS7\teIF3-S8\nmRpS7\tSrp19\nmRpS7\tEfTuM\nmRpS7\teIF3-S10\nmRpS7\teIF-2gamma\nmRpS7\tSrp54k\nmRpS7\teRF1\nmRpS7\tTango7\nmRpS7\tRpLP2\nmRpS7\teIF3-S9\nmRpS7\tRpL4\nmRpS7\tRpS10a\nmRpS7\tRpS12\nmRpS7\tRpS10b\nmRpS7\tCG7048\nmRpS7\tRpS15Ab\nmRpS7\tRpS15Aa\nmRpS7\tRpL10Ab\nmRpS7\tRpS14a\nmRpS7\tRpL18A\nmRpS7\tRack1\nmRpS7\tRpS4\nmRpS7\tRpS17\nmRpS7\tRpL13\nmRpS7\tRpS7\nmRpS7\tRpS6\nmRpS7\tRpS8\nmRpS7\tRpS16\nmRpS7\tsta\nmRpS7\tRpS13\nmRpS7\tRpL3\nmRpS7\tRpS23\nmRpS7\tRpS27\nmRpS7\tRpL12\nmRpS7\tRpS18\nmRpS7\tRpS3A\nmRpS7\tRpS2\nmRpS7\tRpS3\nmRpS7\tRpS11\nmRpS7\tRpL11\nmRpS7\tRpL30\nmRpS7\tmRpL44\nmRpS7\tmRpS22\nmRpS7\tmRpS34\nmRpS7\tCG4679\nmRpS7\tmRpS23\nmRpS7\tbonsai\nmRpS7\tmRpS10\nmRpS7\tmRpS35\nmRpS7\tmRpL12\nmRpS7\tmRpS18B\nmRpS7\tmRpS9\nmRpS7\tmRpS5\nPrp8\typs\nPrp8\tSrp54\nPrp8\tCG10777\nPrp8\tCG10077\nPrp8\tLS2\nPrp8\tmago\nPrp8\tHrb98DE\nPrp8\tCG16941\nPrp8\tTango4\npont\tTcp-1zeta\npont\tTcp-1eta\npont\tRpt6\npont\tUch-L5\nmRpL44\tmRpS34\nmRpL44\tmRpL12\nmRpL44\tbonsai\nmRpS22\tCG4882\nmRpS22\tmRpL12\nmRpS22\tmRpS10\nmRpS22\tmRpL44\nmRpS22\tmRpS34\nmRpS22\tmRpS23\nmRpS22\tbonsai\nmRpS22\tmRpS18B\nmRpS22\tmRpS9\nCp1\tEct3\nCp1\tPyK\nCp1\tRpn5\nCp1\tCG6891\nCp1\tCalpB\nCp1\tRpn6\nCp1\tCat\nCp1\tRpt5\ncathD\tCG4572\ncathD\tCG6891\ncathD\tPyK\ncathD\tCG8258\ncathD\tEct3\ncathD\tPfk\ncathD\tRpn5\ncathD\tCalpB\ncathD\tRpt5\ncathD\tRpn6\ncathD\tCat\ncathD\tSap-r\ncathD\tCp1\nTango4\tCG10777\nTango4\tLS2\nTango4\tmago\nU2A\typs\nU2A\tCG10077\nU2A\tCG10777\nU2A\tSrp54\nU2A\tHrb98DE\nU2A\tmago\nU2A\tLS2\nU2A\tCG16941\nU2A\tPrp8\nU2A\tTango4\nUba2\tUsp5\nUba2\tUbqn\nsmt3\ttsr\nsmt3\tmago\nsmt3\tUfd1-like\nsmt3\tCG10777\nsmt3\tCG10077\nsmt3\tRae1\nsmt3\tUba2\nzip\tsqh\nND-15\tCG11752\nND-15\tND-51L1\nND-15\tCOX5A\nND-15\tND-51\nND-15\tNP15.6\nND-15\tND-B16.6\nND-15\tND-PDSW\npAbp\trin\npAbp\tRpL3\npAbp\tRpL4\npAbp\temb\npAbp\tDp1\npAbp\tRpS6\npAbp\ttyf\npAbp\tbel\npAbp\typs\npAbp\tCG11505\npAbp\tme31B\neIF4G\tDp1\neIF4G\tkra\neIF4G\tbel\neIF4G\tPlexA\neIF4G\tme31B\neIF4G\tRpL18A\neIF4G\tRpL12\neIF4G\tRpS12\neIF4G\tHsp67Bc\neIF4G\tRpS4\neIF4G\tRpL11\neIF4G\tRpS23\neIF4G\tRpS7\neIF4G\tRpS27\neIF4G\tRpLP2\neIF4G\tRpS10b\neIF4G\tRpS10a\neIF4G\tsta\neIF4G\tRpS16\neIF4G\tRpL4\neIF4G\tRpL13\neIF4G\tRpS18\neIF4G\tRpS13\neIF4G\tRpL3\neIF4G\tRpL10Ab\neIF4G\tRpS11\neIF4G\tRpS8\neIF4G\tRpS2\neIF4G\tHsp27\neIF4G\teIF-3p66\neIF4G\tmago\neIF4G\tTango7\neIF4G\tRpS3\neIF4G\tCG5642\neIF4G\tCG9769\neIF4G\tTrip1\neIF4G\tRbp2\neIF4G\tRpS6\neIF4G\talpha-Cat\neIF4G\teIF3-S10\neIF4G\teIF3-S8\neIF4G\teIF3-S9\neIF4G\tpAbp\nPfk\tCG11876\nPfk\tCG6891\nPfk\tEct3\nPfk\tRpt5\nPfk\tCp1\nPfk\tRpn6\nPfk\tCalpB\nPfk\tRpn5\nPfk\tCat\nPfk\tEno\nPfk\tTpi\nPfk\tPyK\nbonsai\tRpS14a\nbonsai\tRpS16\nbonsai\tsta\nbonsai\tRpS6\nCG12262\tCG10932\nyip2\tUch\nyip2\tAcCoAS\nyip2\tCG10932\nyip2\tCG3902\nyip2\tCG12262\nRab5\tMgstl\nRab5\tCG32683\nRab5\tOst48\nRab5\tCG3529\nRab5\tArr2\nGdi\tCat\nGdi\tCG6891\nGdi\tPfk\nGdi\tRpt1\nGdi\tRpt5\nGdi\tRpn5\nGdi\tCG8258\nGdi\tRpn6\nGdi\tRpn3\nGdi\tPyK\nGdi\tTER94\nGdi\tRhoGDI\nGdi\tRab5\nmRpS10\tSrp54k\nmRpS10\tCG4882\nmRpS10\tRpL10Ab\nmRpS10\tRpS14a\nmRpS10\tRpS16\nmRpS10\tRpS10b\nmRpS10\tRpS13\nmRpS10\tRpS2\nmRpS10\tRpS15Aa\nmRpS10\tRpL3\nmRpS10\tRpL12\nmRpS10\tRpL13\nmRpS10\tRpS4\nmRpS10\tRpS17\nmRpS10\tRpS6\nmRpS10\tsta\nmRpS10\tmRpS23\nmRpS10\tmRpS18B\nmRpS10\tmRpS34\nmRpS10\tmRpL12\nmRpS10\tmRpL44\nmRpS10\tbonsai\nmRpS10\tmRpS9\nSrp54k\tbonsai\nSrp54k\tRpS14a\nSrp54k\tRpS15Aa\nSrp54k\tRpS17\nSrp54k\tRpL10Ab\nSrp54k\tRpS10b\nSrp54k\tRpL12\nSrp54k\tRpS4\nSrp54k\tRpS6\nSrp54k\tRpS16\nSrp54k\tRpS12\nSrp54k\tsta\nSrp54k\tSrp19\nmRpL12\tRpS14a\nmRpL12\tRpL10Ab\nmRpL12\tRpS16\nmRpL12\tsta\nmRpL12\tmRpS34\nmRpL12\tRpS15Aa\nmRpL12\tRpL12\nmRpL12\tbonsai\nTctp\tRpS10b\nTctp\teEF1delta\nTctp\tRpL10Ab\nTctp\tRpS14a\nTctp\tRpS15Aa\nTctp\tRpS4\nTctp\tRpS17\nTctp\tsta\nTctp\tRpS6\nTctp\tRpS16\nTctp\tRpL12\nTctp\tRpS13\nTctp\tRpS12\nTctp\tRpS2\nTctp\tRpL13\neIF-2gamma\tTcp-1zeta\neIF-2gamma\tTcp-1eta\neIF-2gamma\tCG10576\neIF-2gamma\tCctgamma\neIF-2gamma\tCG7033\neIF-2gamma\tpAbp\neIF-2gamma\tCct5\neIF-2gamma\tRpS14a\neIF-2gamma\tCG8258\neIF-2gamma\tCG33123\neIF-2gamma\tCG1354\neIF-2gamma\tRpS15Ab\neIF-2gamma\tRpS15Aa\neIF-2gamma\tT-cp1\neIF-2gamma\tRpS17\neIF-2gamma\tRpL10Ab\neIF-2gamma\tRpLP2\neIF-2gamma\tRpL18A\neIF-2gamma\tRbp2\neIF-2gamma\tCG5642\neIF-2gamma\tRpL30\neIF-2gamma\teIF-3p66\neIF-2gamma\teIF4G\neIF-2gamma\tRpS3A\neIF-2gamma\tCG9769\neIF-2gamma\tRpL12\neIF-2gamma\tRpL11\neIF-2gamma\tRpS8\neIF-2gamma\tRpS10a\neIF-2gamma\tRpS10b\neIF-2gamma\tRpL13\neIF-2gamma\tRpS27\neIF-2gamma\tRpS23\neIF-2gamma\tRpS18\neIF-2gamma\tRpS11\neIF-2gamma\tRpS7\neIF-2gamma\tRpL3\neIF-2gamma\tRpS12\neIF-2gamma\tRpL4\neIF-2gamma\tRpS4\neIF-2gamma\tRpS13\neIF-2gamma\tTrip1\neIF-2gamma\tRpS2\neIF-2gamma\teIF3-S10\neIF-2gamma\tRpS16\neIF-2gamma\tRpS6\neIF-2gamma\tTango7\neIF-2gamma\tsta\neIF-2gamma\tRpS3\neIF-2gamma\teIF3-S8\neIF-2gamma\teIF3-S9\nUch-L5\tUsp5\nUch-L5\tRpt4R\nUch-L5\tRpn12\nRpt4R\tRpn12\nProsalpha1\tRpt4R\nProsalpha1\tRpn2\nProsalpha1\tRpn12\nProsalpha1\tRpn6\nProsalpha1\tRpt6R\nProsalpha1\tRpn5\nProsalpha1\tRpt6\nProsalpha1\tRpt5\nProsalpha1\tProsalpha6\nProsalpha1\tRpt2\nProsalpha1\tProsbeta7\nmRpS23\tCG4882\nmRpS23\tmRpL44\nmRpS23\tmRpL12\nmRpS23\tbonsai\nmRpS23\tmRpS34\nmRpS18B\tCG4882\nmRpS18B\tmRpL12\nmRpS18B\tbonsai\nmRpS18B\tmRpS34\nmRpS18B\tmRpL44\nmRpS18B\tmRpS23\nmor\tpont\nBap55\tUch-L5\nBap55\tpont\nBap55\tmor\nade5\tCG3011\nCg25C\tLanA\nvkg\tGlt\nvkg\tLanA\nvkg\tNdg\nvkg\ttrol\nvkg\tCg25C\nUfd1-like\tUbqn\nUfd1-like\tUsp5\nTER94\tpoe\nTER94\tbel\nTER94\tCnx99A\nTER94\tERp60\nTER94\tPdi\nTER94\tsmt3\nTER94\tHop\nTER94\tHsc70-3\nTER94\tCalpB\nTER94\tPfk\nTER94\tCG6891\nTER94\tEct3\nTER94\tPyK\nTER94\tRpn2\nTER94\tCat\nTER94\tCp1\nTER94\tme31B\nTER94\tCG8258\nTER94\tcathD\nTER94\tUbqn\nTER94\tRpn5\nTER94\tRpn6\nTER94\tRpt1\nTER94\tRpt5\nTER94\tCG8209\nTER94\tRpt2\nTER94\tp47\nTER94\tUfd1-like\nade3\tRpS15Aa\nade3\tRpL12\nade3\tRpS14a\nade3\tRpL10Ab\nade3\tsu(r)\nade3\tRpS6\nade3\tRpS4\nade3\tade5\nade3\tCG3011\nmRpS9\tCG4882\nmRpS9\tade3\nmRpS9\tRpS14a\nmRpS9\tRpL10Ab\nmRpS9\tRpS16\nmRpS9\tsta\nmRpS9\tRpS13\nmRpS9\tRpS15Aa\nmRpS9\tRpL12\nmRpS9\tRpS2\nmRpS9\tmRpS34\nmRpS9\tmRpL44\nmRpS9\tmRpS18B\nmRpS9\tmRpS23\nmRpS9\tbonsai\nmRpS9\tmRpL12\nmRpS5\tCG11876\nmRpS5\tRpS16\nmRpS5\tRpS14a\nmRpS5\tRpL10Ab\nmRpS5\tRpLP2\nmRpS5\tCG4882\nmRpS5\tSrp54k\nmRpS5\tRpS10b\nmRpS5\tRpL4\nmRpS5\tRpS10a\nmRpS5\tRpS8\nmRpS5\tRpS13\nmRpS5\tsta\nmRpS5\tRpL3\nmRpS5\tRpL12\nmRpS5\tRpS15Aa\nmRpS5\tRpS23\nmRpS5\tRpS15Ab\nmRpS5\tRpS27\nmRpS5\tRpS7\nmRpS5\tEfTuM\nmRpS5\tRpL18A\nmRpS5\tRpL13\nmRpS5\tRpS4\nmRpS5\tRpS17\nmRpS5\tRpS6\nmRpS5\tRpS18\nmRpS5\tRpS11\nmRpS5\tRpL11\nmRpS5\tRpS3\nmRpS5\tmRpS34\nmRpS5\tmRpS18B\nmRpS5\tmRpS23\nmRpS5\tmRpS22\nmRpS5\tCG4679\nmRpS5\tmRpL44\nmRpS5\tmRpL12\nmRpS5\tmRpS10\nmRpS5\tbonsai\nmRpS5\tmRpS9\nNot1\tIswi\nNot1\teIF4G\nNot1\tpAbp\nNot1\tPrp8\nNot1\tme31B\nNot3\tfrj\nNot3\tme31B\nNot3\tNot1\nHsc70-3\tCG2852\nHsc70-3\tHsp60\nHsc70-3\tOst48\nHsc70-3\tRae1\nHsc70-3\tCG2918\nPyK\tbel\nPyK\tpoe\nPyK\tme31B\nPyK\tMen\nPyK\tAldh\nPyK\tEct3\nPyK\tCG6891\nPyK\tCalpB\nPyK\tND-51\nPyK\tRpn5\nPyK\tVha68-2\nPyK\tCat\nPyK\tMdh2\nPyK\tEno\nLanA\tLanB2\nalphaCOP\tdeltaCOP\nSrp19\tRpS14a\nSrp19\tRpS15Aa\nSrp19\tRpS17\nSrp19\tRpL10Ab\nSrp19\tRpS10b\nSrp19\tRpS6\nSrp19\tRpS4\nSrp19\tsta\nSrp19\tRpS12\nSrp19\tRpL12\nSrp19\tRpS16\nSrp72\tRpS12\nSrp72\tRpS10b\nSrp72\tRpS6\nSrp72\tRpS13\nSrp72\tRpL10Ab\nSrp72\tRpL13\nSrp72\tsta\nSrp72\tRpS4\nSrp72\tRpS2\nSrp72\tRpL3\nSrp72\tRpS16\nSrp72\tRpL12\nSrp72\tSrp54k\nSrp72\tSrp19\nHsp83\tTcp-1zeta\nHsp83\tRack1\nHsp83\tzip\nHsp83\tERp60\nHsp83\tpont\nHsp83\tPdi\nHsp83\tTpi\nHsp83\tHDAC6\nHsp83\tbai\nHsp83\tCctgamma\nHsp83\tCnx99A\nHsp83\tCG2918\nHsp83\tCct5\nHsp83\tCG7033\nHsp83\tSgt\nHsp83\tHsp27\nHsp83\tT-cp1\nHsp83\tCalpB\nHsp83\tEct3\nHsp83\tCG6891\nHsp83\tDlic\nHsp83\tcpa\nHsp83\tCp1\nHsp83\tDhc64C\nHsp83\tHsp60C\nHsp83\tcpb\nHsp83\tcathD\nHsp83\tPfk\nHsp83\tGdi\nHsp83\tRac1\nHsp83\tPyK\nHsp83\tRpn6\nHsp83\tHsp60\nHsp83\tRpn5\nHsp83\tRpt1\nHsp83\tRpn3\nHsp83\tRpt5\nHsp83\tHsp70Ab\nHsp83\tCat\nHsp83\tCG8258\nHsp83\tHsc70-5\nHsp83\tFKBP59\nHsp83\tTER94\nHsp83\tDnaJ-H\nHsp83\tHsc70-3\nHsp83\tCG1416\nHsp83\tHop\nCG11089\tade5\nCG11089\tade3\nCG11089\tCG3011\nERp60\tPdi\nERp60\tOst48\nERp60\tCG2852\nERp60\tCG2918\nERp60\tCaBP1\nERp60\tHsc70-3\nERp60\tGp93\nzetaCOP\tKlp10A\nzetaCOP\tDlic\nzetaCOP\tp115\nzetaCOP\tcpa\nzetaCOP\talphaCOP\nzetaCOP\tbetaCOP\nzetaCOP\tdeltaCOP\nCOX6B\tND-B16.6\nCG7048\tcpa\nCG7048\tSrp19\nCG7048\tNlp\nCG7048\tmRpS23\nCG7048\tCG8498\nCG7048\trobl\nCG7048\tTim9a\nCG7048\teEF1delta\nCG7770\tRpS18\nCG7770\tpont\nCG7770\tCG7048\narm\tHsc70-3\narm\tProsalpha6\narm\tUch-L5\narm\tRpn5\narm\tProsbeta7\narm\tRpn12\narm\tRpt6R\narm\tRpt6\narm\tsyd\narm\tRpn2\narm\tRpn6\narm\tRpt1\narm\tRpt2\narm\tRpt5\narm\temb\narm\tRpt4R\narm\tpont\narm\talpha-Cat\neIF-3p66\tTcp-1eta\neIF-3p66\tCct5\neIF-3p66\tCctgamma\neIF-3p66\tRpS12\neIF-3p66\tRpS16\neIF-3p66\tRpS11\neIF-3p66\tRpS27\neIF-3p66\tRpS23\neIF-3p66\tRpS10a\neIF-3p66\tRpS10b\neIF-3p66\tRpS13\neIF-3p66\tRpL10Ab\neIF-3p66\tRpL13\neIF-3p66\tRpS7\neIF-3p66\tRpL12\neIF-3p66\tsta\neIF-3p66\tRpS4\neIF-3p66\tRpL11\neIF-3p66\tRpS18\neIF-3p66\tRpS2\neIF-3p66\tRpL3\neIF-3p66\tRpL18A\neIF-3p66\tRpS3\neIF-3p66\tRpL4\neIF-3p66\tRpS6\neIF-3p66\tCG9769\neIF-3p66\teIF3-S10\neIF-3p66\tTango7\neIF-3p66\teIF3-S8\neIF-3p66\teIF3-S9\neIF-3p66\tTrip1\neIF-3p66\tCG5642\nCG11752\tND-B16.6\nCOX4\tCG11752\nCOX4\tporin\nCOX4\tND-B16.6\nCOX4\tND-PDSW\nCOX4\tCyt-c-p\nCOX4\tCOX6B\nCOX5A\tsun\nCOX5A\tND-51\nCOX5A\tCG7603\nCOX5A\tCG11752\nCOX5A\tND-B16.6\nCOX5A\tCyt-c-p\nCOX5A\tND-PDSW\nCOX5A\tCOX6B\nCOX5A\tCOX4\nEno\tHsc70-3\nEno\tsta\nTpi\tCat\nTpi\tCG11899\nTpi\tRpS13\nTpi\tCG11876\nTpi\tade3\nTpi\tCG3011\nTpi\tHsc70-3\nTpi\tMdh2\nTpi\tPyK\nTpi\tEno\nCctgamma\tTrip1\nCctgamma\tCG10576\nCctgamma\tCG3800\nCctgamma\tHop\nCctgamma\tTcp-1zeta\nCctgamma\tTcp-1eta\nCctgamma\tCG7033\nRpt1\tUch\nRpt1\tRpS15Aa\nRpt1\tRpS15Ab\nRpt1\tRpS6\nRpt1\tUbqn\nRpt1\tRpL3\nRpt1\tRpS11\nRpt1\tRpS18\nRpt1\tUfd1-like\nRpt1\tCct5\nRpt1\tCalpB\nRpt1\tEct3\nRpt1\tCG6891\nRpt1\tPyK\nRpt1\tCp1\nRpt1\tCat\nRpt1\tPfk\nRpt1\tCG8258\nRpt1\tcathD\nRpt1\tProsalpha1\nRpt1\tUch-L5\nRpt1\tRpt6R\nRpt1\tRpt4R\nRpt1\tProsalpha6\nRpt1\tRpn12\nRpt1\tRpt5\nRpt1\tRpn5\nRpt1\tProsbeta7\nRpt1\tRpn2\nRpt1\tRpt6\nRpt1\tRpt2\nRpt1\tRpn6\nDlic\tp115\nDlic\tdeltaCOP\nDlic\tcpa\nDhc64C\tCLIP-190\nDhc64C\tbetaTub56D\nDhc64C\tKlc\nDhc64C\tpoe\nDhc64C\tbai\nDhc64C\tFKBP59\nDhc64C\tbetaCOP\nDhc64C\tdeltaCOP\nDhc64C\tepsilonCOP\nDhc64C\tbeta'COP\nDhc64C\tCG10186\nDhc64C\tzetaCOP\nDhc64C\tp115\nDhc64C\tcpa\nDhc64C\tcpb\nDhc64C\trobl\nDhc64C\tKhc\nDhc64C\tBicD\nDhc64C\tDlic\nEf1beta\tFeCH\nEf1beta\tCG7048\nEf1beta\tAats-asp\nEf1beta\tRpS15Aa\nEf1beta\tRpS10a\nEf1beta\tRpS15Ab\nEf1beta\tCG5642\nEf1beta\tRpL3\nEf1beta\tRpS3A\nEf1beta\tRpS12\nEf1beta\tRpS13\nEf1beta\tRpS10b\nEf1beta\tRpS16\nEf1beta\tRpS2\nEf1beta\tRpS6\nEf1beta\tRpL12\nEf1beta\tRpL30\nEf1beta\tRpS23\nEf1beta\tRpS11\nEf1beta\teEF1delta\nEf1beta\tRpL13\nEf1beta\tsta\nEf1beta\tRpS7\nEf1beta\tTctp\nEf1beta\tTrip1\nEf1beta\tRpS8\nEf1beta\tRpS18\nEf1beta\tRack1\nEf1beta\tRpS17\nEf1beta\tRpL10Ab\nEf1beta\tRpL18A\nEf1beta\tRpS3\nEf1beta\tRpS4\nEf1beta\tRpL11\nEf1beta\tRpLP2\nEf1beta\tRpL4\nRpn13\tTER94\nRpn13\tUfd1-like\nRpn13\tUbqn\nRpn13\tProsalpha6\nRpn13\tProsbeta7\nRpn13\tRpt4R\nRpn13\tProsalpha7\nRpn13\tRpn12\nRpn13\tRpt6R\nRpn13\tRpn5\nRpn13\tRpt6\nRpn13\tRpn6\nRpn13\tUch-L5\nRpn13\tRpt2\nRpn13\tRpt5\nRpn13\tRpt1\nRpn13\tRpn2\nTango7\tCG7033\nTango7\tTcp-1zeta\nTango7\tmRpS10\nTango7\tRpn5\nTango7\tbonsai\nTango7\tRpn2\nTango7\tRpn6\nTango7\tRpS14a\nTango7\tRpS15Aa\nTango7\tRpS17\nTango7\tRpL10Ab\nTango7\tRpL12\nTango7\tRpL13\nTango7\tRpL18A\nTango7\tRpL3\nTango7\tRpS10a\nTango7\tRpL11\nTango7\tRpS10b\nTango7\tRpS2\nTango7\tRpS27\nTango7\tRpS23\nTango7\tRpS18\nTango7\tRpS12\nTango7\tRpS13\nTango7\tRpS4\nTango7\tRpS6\nTango7\tRpS16\nTango7\tsta\nTango7\teIF3-S10\nTango7\tRpS3\nTango7\tTrip1\nTango7\tRpL4\nTango7\tCG9769\nTango7\teIF3-S9\nTango7\tCG5642\nTango7\teIF3-S8\nRpt2\tUbqn\nRpt2\tUfd1-like\nRpt2\tCG8209\nRpt2\tUch-L5\nRpt2\tRpt4R\nRpt2\tRpn12\nRpt2\tRpt5\nRpt2\tRpt6\nRpt2\tProsalpha6\nRpt2\tRpn5\nRpt2\tProsbeta7\nRpn5\tUbqn\nRpn5\tUfd1-like\nRpn5\tCat\nRpn5\tCalpB\nRpn5\tCG6891\nRpn5\tUch-L5\nRpn5\tRpt4R\nRpn5\tRpt6\nRpn5\tRpn12\nbetaCOP\tcpa\nbetaCOP\tDlic\nbetaCOP\tKlp10A\nbetaCOP\tp115\nbetaCOP\talphaCOP\nbetaCOP\tdeltaCOP\nRpt6R\tRpS4\nRpt6R\tpont\nRpt6R\tRpt6\nRpt6R\tUch-L5\nRpt6R\tRpt4R\nRpt6R\tProsalpha6\nRpt6R\tRpt2\nRpt6R\tRpt5\nRpt6R\tProsbeta7\nRpt6R\tRpn12\nRpt6R\tRpn2\nRpt6R\tRpn5\nRpn2\tTrip1\nRpn2\tRpS2\nRpn2\tUsp5\nRpn2\tUch-L5\nRpn2\tProsalpha6\nRpn2\tRpt4R\nRpn2\tProsbeta7\nRpn2\tRpn12\nRpn2\tRpn5\nRpn2\tRpt6\nRpn2\tRpt2\nRpn2\tRpt5\nRpL10Ab\tbonsai\nRpL10Ab\tmago\nRpL10Ab\tCG5642\nRpL10Ab\tRpS15Aa\nRpL10Ab\tRpS14a\nRpL10Ab\tRpL12\nRpL10Ab\tRpS16\nRpL10Ab\tRpS12\nRpL10Ab\tRpS10b\nRpL10Ab\tRpS4\nRpL10Ab\tsta\nRpL10Ab\tRpS6\nTrip1\tRpS14a\nTrip1\tTcp-1zeta\nTrip1\tCG10576\nTrip1\tRpS17\nTrip1\tCG7033\nTrip1\tRpL10Ab\nTrip1\tRpS10b\nTrip1\tRpL12\nTrip1\tRpS12\nTrip1\tRpS4\nTrip1\tRpS6\nTrip1\tRpS16\nTrip1\tsta\nTrip1\tCG9769\nTrip1\teIF3-S10\nTrip1\tCG5642\ncpa\tp115\ncpa\tdeltaCOP\ncpb\tCG6891\ncpb\tflr\ncpb\tp115\ncpb\tdeltaCOP\ncpb\ttsr\ncpb\tbetaCOP\ncpb\tzetaCOP\ncpb\tDlic\ncpb\tcpa\neIF3-S10\tCG33123\neIF3-S10\tAats-ile\neIF3-S10\tRpS14a\neIF3-S10\tRpS17\neIF3-S10\tRpS15Aa\neIF3-S10\tRpL10Ab\neIF3-S10\tRpL12\neIF3-S10\tRpS12\neIF3-S10\tRpS10b\neIF3-S10\tRpS6\neIF3-S10\tRpS16\neIF3-S10\tRpS4\neIF3-S10\tsta\neIF3-S10\tCG5642\neIF3-S10\tCG9769\neIF3-S9\tCctgamma\neIF3-S9\trin\neIF3-S9\tRpS14a\neIF3-S9\tRpS15Aa\neIF3-S9\tRpS17\neIF3-S9\tTcp-1eta\neIF3-S9\tCG7033\neIF3-S9\tT-cp1\neIF3-S9\tCG10576\neIF3-S9\tpAbp\neIF3-S9\tRpL10Ab\neIF3-S9\tRpL12\neIF3-S9\tRpS12\neIF3-S9\tRpS10a\neIF3-S9\tRpS27\neIF3-S9\tRpS10b\neIF3-S9\tRpS18\neIF3-S9\tRpL11\neIF3-S9\tRpL13\neIF3-S9\tRpL3\neIF3-S9\tRpL4\neIF3-S9\tRpS16\neIF3-S9\tsta\neIF3-S9\tRpS2\neIF3-S9\tRpS13\neIF3-S9\tRpS4\neIF3-S9\tRpS6\neIF3-S9\tRpS3\neIF3-S9\tCG5642\neIF3-S9\teIF3-S8\neIF3-S9\tCG9769\neIF3-S9\tTrip1\neIF3-S9\teIF3-S10\nPdi\tUgt\nPdi\tOst48\nPdi\tCG2852\nPdi\tCG2918\nPdi\tHsc70-3\nepsilonCOP\tDlic\nepsilonCOP\tKlc\nepsilonCOP\tKlp10A\nepsilonCOP\tcpb\nepsilonCOP\tcpa\nepsilonCOP\tp115\nepsilonCOP\tdeltaCOP\nepsilonCOP\tzetaCOP\nepsilonCOP\talphaCOP\nepsilonCOP\tbetaCOP\nbeta'COP\tcpb\nbeta'COP\tcpa\nbeta'COP\tDlic\nbeta'COP\tKlp10A\nbeta'COP\tKlc\nbeta'COP\tp115\nbeta'COP\tzetaCOP\nbeta'COP\talphaCOP\nbeta'COP\tdeltaCOP\nbeta'COP\tbetaCOP\nbeta'COP\tepsilonCOP\nCaBP1\tUgt\nCaBP1\tOst48\nCaBP1\tCG2918\nCaBP1\tHsc70-3\nCaBP1\tPdi\nGp93\tCctgamma\nGp93\tbai\nGp93\tHsp27\nGp93\tHop\nGp93\tHsp70Ab\nGp93\tCG11577\nGp93\tPdi\nGp93\tHsc70-3\nGp93\tCG2918\nGp93\tCaBP1\nTcp-1eta\tRpL12\nTcp-1eta\tRpS6\nTcp-1eta\tRpL13\nTcp-1eta\tRpL10Ab\nTcp-1eta\tsta\nTcp-1eta\tRpS2\nTcp-1eta\tRpS12\nTcp-1eta\tRpS13\nTcp-1eta\tCG10576\nTcp-1eta\tRpS4\nTcp-1eta\tRpS16\nTcp-1eta\tTrip1\nTcp-1eta\tHop\nTcp-1eta\tade3\nTcp-1eta\tTcp-1zeta\nTcp-1eta\tCG7033\nRpL13\tmRpL12\nRpL13\tCG7033\nRpL13\tade3\nRpL13\tbonsai\nRpL13\tSrp19\nRpL13\tmago\nRpL13\tCG5642\nRpL13\teIF3-S10\nRpL13\tCG10576\nRpL13\tSrp54k\nRpL13\tCG9769\nRpL13\tTrip1\nRpL13\tRpS15Aa\nRpL13\tRpL12\nRpL13\tRpS12\nRpL13\tRpS13\nRpL13\tRpS6\nRpL13\tRpS14a\nRpL13\tRpL10Ab\nRpL13\tRpS4\nRpL13\tsta\nRpL13\tRpS17\nRpL13\tRpS10b\nRpL13\tRpS16\nRpt4\tProsalpha1\nRpt4\tarm\nRpt4\tRpt4R\nRpt4\tUch-L5\nRpt4\tRpt6R\nRpt4\tRpn13\nRpt4\tRpt1\nRpt4\tRpt6\nRpt4\tProsalpha6\nRpt4\tProsbeta7\nRpt4\tRpn6\nRpt4\tRpt2\nRpt4\tRpn3\nRpt4\tProsalpha7\nRpt4\tRpn2\nRpt4\tRpn12\nRpt4\tRpn5\nRpt4\tRpt5\nRpS11\tvig\nRpS11\tbic\nRpS11\tCG7033\nRpS11\tTctp\nRpS11\tTcp-1eta\nRpS11\teEF1delta\nRpS11\tmago\nRpS11\tEfTuM\nRpS11\tSrp72\nRpS11\tbonsai\nRpS11\tmRpS10\nRpS11\tCG5642\nRpS11\tmRpS9\nRpS11\tmRpL12\nRpS11\teIF3-S8\nRpS11\tCG9769\nRpS11\teIF3-S10\nRpS11\tSrp19\nRpS11\tade3\nRpS11\teIF3-S9\nRpS11\tSrp54k\nRpS11\tTango7\nRpS11\tTrip1\nRpS11\tRpL10Ab\nRpS11\tRpS23\nRpS11\tRpL3\nRpS11\tRpL12\nRpS11\tRpL11\nRpS11\tRpS14a\nRpS11\tRpL4\nRpS11\tRpS18\nRpS11\tRpS6\nRpS11\tsta\nRpS11\tRpS10a\nRpS11\tRpS17\nRpS11\tRpS2\nRpS11\tRpL18A\nRpS11\tRpS12\nRpS11\tRpS3\nRpS11\tRpS13\nRpS11\tRpS27\nRpS11\tRpS10b\nRpS11\tRpL13\nRpS11\tRpS16\nRpS11\tRpS15Aa\nRpS11\tRpS4\neIF3-S8\tCG33123\neIF3-S8\tTcp-1zeta\neIF3-S8\teEF1delta\neIF3-S8\tpAbp\neIF3-S8\tRpS14a\neIF3-S8\tRpS17\neIF3-S8\tRpS15Aa\neIF3-S8\tCG10576\neIF3-S8\tRpL12\neIF3-S8\tRpL10Ab\neIF3-S8\tRpL11\neIF3-S8\tRpL13\neIF3-S8\tRpL3\neIF3-S8\tRpS12\neIF3-S8\tsta\neIF3-S8\tRpS2\neIF3-S8\tRpS13\neIF3-S8\tRpS4\neIF3-S8\tRpS10b\neIF3-S8\tRpS27\neIF3-S8\tRpS10a\neIF3-S8\tRpS16\neIF3-S8\tRpL4\neIF3-S8\tRpS18\neIF3-S8\tRpS6\neIF3-S8\tRpS3\neIF3-S8\teIF3-S10\neIF3-S8\tTrip1\neIF3-S8\tCG9769\neIF3-S8\tCG5642\nHop\tHsp60\nHop\tHsc70-3\nHop\tUbqn\nCG1416\tCG7033\nCG1416\tHsp60\nCG1416\tGp93\nCG1416\tCctgamma\nCG1416\tHop\nRpS6\tRpS14a\nRpS6\tsta\nRpS2\tCG10576\nRpS2\tade3\nRpS2\tbonsai\nRpS2\tCG7033\nRpS2\tmago\nRpS2\tmRpL12\nRpS2\tCG5642\nRpS2\teIF3-S10\nRpS2\tSrp19\nRpS2\tCG9769\nRpS2\tSrp54k\nRpS2\tTrip1\nRpS2\tRpS6\nRpS2\tRpS15Aa\nRpS2\tRpL12\nRpS2\tRpS14a\nRpS2\tRpS10b\nRpS2\tRpS16\nRpS2\tRpL10Ab\nRpS2\tRpS17\nRpS2\tsta\nRpS2\tRpS4\nRpS2\tRpS13\nRpS2\tRpL13\nRpS2\tRpS12\nProsalpha6\tTrip1\nProsalpha6\tTcp-1zeta\nProsalpha6\tUch-L5\nProsalpha6\tRpn5\nProsalpha6\tRpt4R\nProsalpha6\tRpt6\nProsalpha6\tProsbeta7\nProsalpha6\tRpn12\nND-PDSW\tCOX6B\nND-PDSW\tCG7603\nND-PDSW\tND-B16.6\nND-PDSW\tCG11752\nNP15.6\tCG7603\nNP15.6\tCOX4\nNP15.6\tCG11752\nNP15.6\tND-51\nNP15.6\tCOX5A\nNP15.6\tND-B16.6\nNP15.6\tND-PDSW\nKlc\tDlic\nKlc\tbetaCOP\nKlc\tdeltaCOP\nKlc\tzetaCOP\nKlc\tKlp10A\nKhc\tDlic\nKhc\tzetaCOP\nKhc\tepsilonCOP\nKhc\tbai\nKhc\tdeltaCOP\nKhc\tbeta'COP\nKhc\tsyd\nKhc\tbetaCOP\nKhc\tKlp10A\nKhc\tbetaTub56D\nKhc\tKlc\nRpL12\tbonsai\nRpL12\tmago\nRpL12\tRpS16\nRpL12\tsta\nRpL12\tRpS14a\nRpL12\tRpS6\nRpt5\tUbqn\nRpt5\tCat\nRpt5\tCalpB\nRpt5\tCG6891\nRpt5\tEct3\nRpt5\tPyK\nRpt5\tTcp-1eta\nRpt5\tUfd1-like\nRpt5\tUch-L5\nRpt5\tRpt4R\nRpt5\tProsalpha6\nRpt5\tProsbeta7\nRpt5\tRpn5\nRpt5\tRpt6\nRpt5\tRpn12\nProsalpha7\tRpS16\nProsalpha7\tCct5\nProsalpha7\tTcp-1eta\nProsalpha7\tCG7033\nProsalpha7\tTrip1\nProsalpha7\tCG8258\nProsalpha7\tarm\nProsalpha7\tUch-L5\nProsalpha7\tRpt4R\nProsalpha7\tProsalpha1\nProsalpha7\tRpt6R\nProsalpha7\tRpn12\nProsalpha7\tRpt6\nProsalpha7\tRpn6\nProsalpha7\tRpt2\nProsalpha7\tRpt1\nProsalpha7\tProsbeta7\nProsalpha7\tProsalpha6\nProsalpha7\tRpn5\nProsalpha7\tRpn2\nProsalpha7\tRpt5\nRpn6\tTrip1\nRpn6\tRpS2\nRpn6\tUbqn\nRpn6\tCalpB\nRpn6\tCat\nRpn6\tPyK\nRpn6\tEct3\nRpn6\tCG6891\nRpn6\tUch-L5\nRpn6\tProsalpha6\nRpn6\tRpt4R\nRpn6\tProsbeta7\nRpn6\tRpn2\nRpn6\tRpt5\nRpn6\tRpn12\nRpn6\tRpn5\nRpn6\tRpt2\nRpn6\tRpt6R\nRpn6\tRpt6\nATPsynbeta\tRack1\nATPsynbeta\tCOX4\nATPsynbeta\tCOX6B\nATPsynbeta\tsta\nATPsynbeta\tHsc70-5\nATPsynbeta\tEno\nATPsynbeta\tRpL4\nATPsynbeta\tRpS3A\nATPsynbeta\tCG11876\nATPsynbeta\tMpcp\nATPsynbeta\tporin\nATPsynbeta\tVha100-1\nATPsynbeta\tRpS2\nATPsynbeta\tVha100-2\nATPsynbeta\tHsp83\nATPsynbeta\tND-51\nATPsynbeta\tVha68-2\nATPsynbeta\tEfTuM\nATPsynbeta\tMdh2\nATPsynbeta\tCG9090\nATPsynbeta\tCOX5A\nATPsynbeta\tsun\nATPsynCF6\tVha100-1\nATPsynCF6\tVha100-2\nATPsynCF6\tNP15.6\nATPsynCF6\tVha68-2\nATPsynCF6\tND-B16.6\nATPsynCF6\tCOX6B\nATPsynCF6\tCG11752\nATPsynCF6\tCyt-c-p\nATPsynCF6\tCOX4\nATPsynCF6\tND-PDSW\nATPsynCF6\tCOX5A\nATPsynCF6\tsun\nATPsynCF6\tATPsynbeta\nCG8258\tRpS13\nCG8258\tRpL11\nCG8258\teIF3-S9\nCG8258\tTango7\nCG8258\tHsp60C\nCG8258\tRpn2\nCG8258\tRpS3\nCG8258\tRpL12\nCG8258\tRpL3\nCG8258\tHsc70-5\nCG8258\tRpL4\nCG8258\tRpS4\nCG8258\tRpS2\nCG8258\tRpS16\nCG8258\tHsp60\nCG8258\tCG10576\nCG8258\tCat\nCG8258\tCG6891\nCG8258\tCalpB\nCG8258\tEct3\nCG8258\tCp1\nCG8258\tPfk\nCG8258\tPyK\nCG8258\tade3\nCG8258\tTrip1\nCG8258\tRpt5\nCG8258\tRpn6\nCG8258\tRpn5\nCG8258\tHop\nCG8258\tCct5\nCG8258\tCctgamma\nCG8258\tTcp-1zeta\nCG8258\tCG7033\nCG8258\tTcp-1eta\nCG8258\tT-cp1\nRpS7\tRpt6R\nRpS7\tRpt6\nRpS7\tvig\nRpS7\teEF1delta\nRpS7\tRpt1\nRpS7\tRpt2\nRpS7\tade3\nRpS7\tmRpS10\nRpS7\tSrp72\nRpS7\tmago\nRpS7\tbonsai\nRpS7\tTctp\nRpS7\tCG5642\nRpS7\tCG9769\nRpS7\teIF3-S10\nRpS7\tSrp54k\nRpS7\tSrp19\nRpS7\tTrip1\nRpS7\teIF3-S9\nRpS7\teIF3-S8\nRpS7\tTango7\nRpS7\tRpS15Ab\nRpS7\tRpS10a\nRpS7\tRpS15Aa\nRpS7\tRpS14a\nRpS7\tRpL4\nRpS7\tRpL13\nRpS7\tsta\nRpS7\tRpS2\nRpS7\tRpS23\nRpS7\tRpS16\nRpS7\tRpL10Ab\nRpS7\tRpS6\nRpS7\tRpS18\nRpS7\tRpL11\nRpS7\tRpS12\nRpS7\tRpS4\nRpS7\tRpS3\nRpS7\tRpS13\nRpS7\tRpS27\nRpS7\tRpL3\nRpS7\tRpS10b\nRpS7\tRpS17\nRpS7\tRpL18A\nRpS7\tRpS11\nRpS7\tRpL12\nRpL10\tmRpS10\nRpL10\tade3\nRpL10\tCct5\nRpL10\tmRpS5\nRpL10\teEF1delta\nRpL10\tmRpL12\nRpL10\tmRpS9\nRpL10\tEf1beta\nRpL10\teIF4G\nRpL10\tSrp72\nRpL10\tmago\nRpL10\tRbp2\nRpL10\teIF3-S10\nRpL10\teIF3-S8\nRpL10\tCG5642\nRpL10\tCG10576\nRpL10\tTango7\nRpL10\tTrip1\nRpL10\tSrp19\nRpL10\tmRpS7\nRpL10\teIF-2gamma\nRpL10\tElf\nRpL10\teIF-3p66\nRpL10\tCG9769\nRpL10\teIF3-S9\nRpL10\tSrp54k\nRpL10\teRF1\nRpL10\tRpS15Ab\nRpL10\tTctp\nRpL10\tRpS14a\nRpL10\tRpS15Aa\nRpL10\tRpS10b\nRpL10\tRpS17\nRpL10\tRpL13\nRpL10\tRpS8\nRpL10\tRpS16\nRpL10\tsta\nRpL10\tRpL12\nRpL10\tRpS3\nRpL10\tRpL4\nRpL10\tRpS27\nRpL10\tRpL3\nRpL10\tRpS12\nRpL10\tRpL11\nRpL10\tRpLP2\nRpL10\tRpL18A\nRpL10\tRpL26\nRpL10\tRpS4\nRpL10\tRack1\nRpL10\tRpS3A\nRpL10\tRpS11\nRpL10\tRpS13\nRpL10\tRpS10a\nRpL10\tRpL30\nRpL10\tRpS2\nRpL10\tRpS18\nRpL10\tRpS23\nRpL10\tRpL10Ab\nRpL10\tRpS6\nRpL10\tRpS7\nAats-asp\tCG5642\nAats-asp\tCG31739\nAats-asp\tCG33123\nAats-asp\tAats-arg\nAats-asp\tAats-ile\nCG33123\tAats-arg\nAats-ile\tCG33123\nAats-ile\tAats-arg\nRpS8\tT-cp1\nRpS8\tvig\nRpS8\tRpn2\nRpS8\tCct5\nRpS8\teEF1delta\nRpS8\tmRpS10\nRpS8\tCG8258\nRpS8\tTcp-1eta\nRpS8\tbonsai\nRpS8\tCG7033\nRpS8\tade3\nRpS8\tCG10576\nRpS8\tSrp72\nRpS8\tmago\nRpS8\tRbp2\nRpS8\tNacalpha\nRpS8\tCG5642\nRpS8\teIF-3p66\nRpS8\tSrp54k\nRpS8\tSrp19\nRpS8\teIF3-S10\nRpS8\tTctp\nRpS8\tTrip1\nRpS8\tTango7\nRpS8\teIF3-S8\nRpS8\tCG9769\nRpS8\teIF3-S9\nRpS8\tRpL13\nRpS8\tRpS17\nRpS8\tRpL11\nRpS8\tRpS12\nRpS8\tRpL4\nRpS8\tRpS3\nRpS8\tRpS27\nRpS8\tRpS18\nRpS8\tRpS2\nRpS8\tRpS13\nRpS8\tRpS7\nRpS8\tRpS6\nRpS8\tsta\nRpS8\tRpL10Ab\nRpS8\tRpS10b\nRpS8\tRpS10a\nRpS8\tRpS14a\nRpS8\tRpS4\nRpS8\tRpL12\nRpS8\tRpS11\nRpS8\tRpL18A\nRpS8\tRpLP2\nRpS8\tRpL3\nRpS8\tRpS23\nRpS8\tRpS15Ab\nRpS8\tRpS15Aa\nRpS8\tRpS16\nRpt6\tUbqn\nRpt6\tRpS4\nRpt6\tUch-L5\nRpt6\tRpt4R\nRpt6\tRpn12\nRpn3\tbel\nRpn3\tUbqn\nRpn3\tCalpB\nRpn3\tEct3\nRpn3\tPfk\nRpn3\tCG6891\nRpn3\tCat\nRpn3\tCp1\nRpn3\tPyK\nRpn3\tarm\nRpn3\tcathD\nRpn3\tCG8258\nRpn3\tProsalpha1\nRpn3\tUch-L5\nRpn3\tTER94\nRpn3\tProsalpha6\nRpn3\tProsbeta7\nRpn3\tProsalpha7\nRpn3\tRpt4R\nRpn3\tRpn2\nRpn3\tRpt6R\nRpn3\tRpt2\nRpn3\tRpn5\nRpn3\tRpt1\nRpn3\tRpt5\nRpn3\tRpn6\nRpn3\tRpn13\nRpn3\tRpn12\nRpn3\tRpt6\nRpL30\tvig\nRpL30\tNacalpha\nRpL30\tCG10576\nRpL30\tSrp72\nRpL30\teIF4G\nRpL30\tbonsai\nRpL30\teIF3-S8\nRpL30\tmago\nRpL30\teIF3-S10\nRpL30\tCG5642\nRpL30\tmRpS5\nRpL30\tRbp2\nRpL30\tmRpS10\nRpL30\teIF-3p66\nRpL30\tTctp\nRpL30\teIF3-S9\nRpL30\tTango7\nRpL30\tCG9769\nRpL30\tTrip1\nRpL30\tSrp54k\nRpL30\tSrp19\nRpL30\tRpS15Aa\nRpL30\tRpS15Ab\nRpL30\tRpS10a\nRpL30\tRpL18A\nRpL30\tRpS27\nRpL30\tRpL12\nRpL30\tRpS12\nRpL30\tRpS18\nRpL30\tRpS17\nRpL30\tRpS2\nRpL30\tRpL11\nRpL30\tRpS7\nRpL30\tRpS14a\nRpL30\tRpS4\nRpL30\tRpS8\nRpL30\tRpS10b\nRpL30\tRpLP2\nRpL30\tRpL13\nRpL30\tRpS16\nRpL30\tRpS3\nRpL30\tRpS13\nRpL30\tRpS11\nRpL30\tRpS6\nRpL30\tRpL3\nRpL30\tRpL10Ab\nRpL30\tRpS23\nRpL30\tRpL4\nRpL30\tsta\nRpS10a\tTctp\nRpS10a\teEF1delta\nRpS10a\tbonsai\nRpS10a\tmRpS10\nRpS10a\tRpS10b\nRpS10a\tSrp72\nRpS10a\tmago\nRpS10a\tCG5642\nRpS10a\tCG9769\nRpS10a\teIF3-S10\nRpS10a\tTrip1\nRpS10a\tSrp19\nRpS10a\tSrp54k\nRpS10a\tRpS15Aa\nRpS10a\tsta\nRpS10a\tRpS2\nRpS10a\tRpS3\nRpS10a\tRpL10Ab\nRpS10a\tRpS4\nRpS10a\tRpS13\nRpS10a\tRpS6\nRpS10a\tRpS12\nRpS10a\tRpS16\nRpS10a\tRpS14a\nRpS10a\tRpS27\nRpS10a\tRpL13\nRpS10a\tRpL12\nRpS10a\tRpS17\nRpS10a\tRpL3\nRpS17\tCG7033\nRpS17\tCG5642\nRpS17\tbonsai\nRpS17\tRpS6\nRpS17\tRpL12\nRpS17\tRpL10Ab\nRpS17\tRpS15Aa\nRpS17\tRpS12\nRpS17\tsta\nRpS17\tRpS14a\nRpS17\tRpS4\nRpS17\tRpS16\nRpS17\tRpS10b\nRpLP0\tATPsynbeta\nRpLP0\tTcp-1zeta\nRpLP0\tT-cp1\nRpLP0\tCG7033\nRpLP0\tEfTuM\nRpLP0\tNacalpha\nRpLP0\tbonsai\nRpLP0\tTcp-1eta\nRpLP0\tmRpS10\nRpLP0\tade3\nRpLP0\tCct5\nRpLP0\tmRpS5\nRpLP0\teEF1delta\nRpLP0\tSrp72\nRpLP0\teIF4G\nRpLP0\tmRpS9\nRpLP0\tRbp2\nRpLP0\tmago\nRpLP0\tCG5642\nRpLP0\teIF3-S8\nRpLP0\tElf\nRpLP0\teIF3-S10\nRpLP0\tCG10576\nRpLP0\tmRpL12\nRpLP0\tmRpS7\nRpLP0\tTctp\nRpLP0\tSrp19\nRpLP0\tTango7\nRpLP0\teIF-2gamma\nRpLP0\teIF-3p66\nRpLP0\tCG9769\nRpLP0\teIF3-S9\nRpLP0\tRpS15Aa\nRpLP0\tSrp54k\nRpLP0\tTrip1\nRpLP0\tRpS15Ab\nRpLP0\teRF1\nRpLP0\tRpS14a\nRpLP0\tRpS10a\nRpLP0\tEf1beta\nRpLP0\tRpS6\nRpLP0\tRpS13\nRpLP0\tRpS23\nRpLP0\tsta\nRpLP0\tRpS11\nRpLP0\tRpS16\nRpLP0\tRpS7\nRpLP0\tRpS27\nRpLP0\tRpL10\nRpLP0\tRpS3\nRpLP0\tRpS10b\nRpLP0\tRpS8\nRpLP0\tRpLP2\nRpLP0\tRpS12\nRpLP0\tRpL3\nRpLP0\tRack1\nRpLP0\tRpL13\nRpLP0\tRpL26\nRpLP0\tRpL30\nRpLP0\tRpL18A\nRpLP0\tRpL4\nRpLP0\tRpL10Ab\nRpLP0\tRpS18\nRpLP0\tRpS3A\nRpLP0\tRpL12\nRpLP0\tRpL11\nRpLP0\tRpS2\nRpLP0\tRpS4\nRpLP0\tRpS17\neRF1\tbonsai\neRF1\tT-cp1\neRF1\teIF3-S8\neRF1\tCG8635\neRF1\tCG7033\neRF1\tCctgamma\neRF1\tmRpS5\neRF1\tmRpS10\neRF1\twuho\neRF1\tRpS15Ab\neRF1\tRpS14a\neRF1\tRpS15Aa\neRF1\teIF3-S10\neRF1\tpAbp\neRF1\tCG10576\neRF1\tRpS17\neRF1\teIF-2gamma\neRF1\tTrip1\neRF1\tmago\neRF1\tRpLP2\neRF1\teIF3-S9\neRF1\teIF4G\neRF1\tRack1\neRF1\tRpS10a\neRF1\tRpS10b\neRF1\tRpL3\neRF1\tRpL10Ab\neRF1\tRpS27\neRF1\tRpS12\neRF1\tRpS7\neRF1\tRpS18\neRF1\tRpL12\neRF1\tRpS23\neRF1\tRpL18A\neRF1\tRpS16\neRF1\tRpL30\neRF1\tRpS11\neRF1\tRpS3\neRF1\tRpS3A\neRF1\tsta\neRF1\tRpS13\neRF1\tRpS6\neRF1\tRpS8\neRF1\tRpL4\neRF1\tRpS4\neRF1\tRpL11\neRF1\tRpS2\neRF1\tRpL13\nElf\tade5\nElf\tmRpS7\nElf\tRpS15Aa\nElf\tRpS15Ab\nElf\tT-cp1\nElf\teIF3-S9\nElf\tCctgamma\nElf\tRpS17\nElf\tCG33123\nElf\tCct5\nElf\tRpS14a\nElf\teIF3-S8\nElf\teIF3-S10\nElf\teIF-2gamma\nElf\tRpL10Ab\nElf\tRpLP2\nElf\tmago\nElf\tRpL13\nElf\tRpL3\nElf\tRpL30\nElf\tRpL18A\nElf\tRpL12\nElf\tRpL11\nElf\tRack1\nElf\tRpL4\nElf\tRpS4\nElf\tRpS10b\nElf\tRpS10a\nElf\tRpS12\nElf\teIF4G\nElf\tRpS11\nElf\tRpS13\nElf\tRpS27\nElf\tRpS8\nElf\tRpS16\nElf\tRpS3\nElf\tsta\nElf\tRpS23\nElf\tRpS18\nElf\tRpS7\nElf\tRpS6\nElf\tRpS3A\nElf\tRpS2\nElf\tpAbp\nElf\teRF1\nRpS15Aa\tbonsai\nRpS15Aa\tRpL12\nRpS15Aa\tsta\nRpS15Aa\tRpS6\nRpS15Aa\tRpS16\nRpS15Aa\tRpS14a\nRpS27\tvig\nRpS27\tSrp72\nRpS27\tmRpS10\nRpS27\tbonsai\nRpS27\tmago\nRpS27\tTctp\nRpS27\tCG5642\nRpS27\tCG9769\nRpS27\tTrip1\nRpS27\teIF3-S10\nRpS27\tSrp19\nRpS27\tSrp54k\nRpS27\tRpS13\nRpS27\tRpS17\nRpS27\tRpS4\nRpS27\tRpS2\nRpS27\tRpL13\nRpS27\tRpL12\nRpS27\tRpS6\nRpS27\tRpS12\nRpS27\tRpS10b\nRpS27\tsta\nRpS27\tRpS16\nRpS27\tRpS14a\nRpS27\tRpL10Ab\nRpS27\tRpL3\nRpS27\tRpS3\nRpS27\tRpS15Aa\nRpS18\tRpt6R\nRpS18\tRpt6\nRpS18\tTcp-1eta\nRpS18\tRpt2\nRpS18\tvig\nRpS18\tade3\nRpS18\tSrp72\nRpS18\tmago\nRpS18\tbonsai\nRpS18\tTctp\nRpS18\tCG5642\nRpS18\tmRpL12\nRpS18\tmRpS10\nRpS18\tmRpS9\nRpS18\teIF3-S10\nRpS18\tCG9769\nRpS18\tSrp19\nRpS18\tSrp54k\nRpS18\tTrip1\nRpS18\tRpS12\nRpS18\tRpS2\nRpS18\tRpS16\nRpS18\tsta\nRpS18\tRpL10Ab\nRpS18\tRpL4\nRpS18\tRpS14a\nRpS18\tRpS17\nRpS18\tRpS3\nRpS18\tRpS4\nRpS18\tRpL3\nRpS18\tRpS10a\nRpS18\tRpS13\nRpS18\tRpS10b\nRpS18\tRpL13\nRpS18\tRpL12\nRpS18\tRpS15Aa\nRpS18\tRpS6\nRpS18\tRpS27\nRpS10b\tbonsai\nRpS10b\tmago\nRpS10b\tRpS15Aa\nRpS10b\tRpS16\nRpS10b\tRpS14a\nRpS10b\tRpL12\nRpS10b\tsta\nRpS10b\tRpS6\nRpS3A\tCG7048\nRpS3A\tNacalpha\nRpS3A\tvig\nRpS3A\teEF1delta\nRpS3A\tCct5\nRpS3A\tCG8258\nRpS3A\tCG7033\nRpS3A\tTcp-1eta\nRpS3A\tSrp72\nRpS3A\tmRpS5\nRpS3A\tRbp2\nRpS3A\tmRpS10\nRpS3A\teIF4G\nRpS3A\tbonsai\nRpS3A\tmago\nRpS3A\tCG5642\nRpS3A\teIF-3p66\nRpS3A\teIF3-S8\nRpS3A\tSrp54k\nRpS3A\teIF3-S10\nRpS3A\tSrp19\nRpS3A\tCG9769\nRpS3A\tade3\nRpS3A\tTrip1\nRpS3A\tTango7\nRpS3A\teIF3-S9\nRpS3A\tTctp\nRpS3A\tRpS12\nRpS3A\tRpS13\nRpS3A\tRpS2\nRpS3A\tRpS17\nRpS3A\tRpS15Ab\nRpS3A\tRpL13\nRpS3A\tRpS15Aa\nRpS3A\tRpS8\nRpS3A\tRpS10a\nRpS3A\tRpS4\nRpS3A\tRpL11\nRpS3A\tRpS23\nRpS3A\tRpS7\nRpS3A\tRpLP2\nRpS3A\tsta\nRpS3A\tRpS6\nRpS3A\tRpL12\nRpS3A\tRpS11\nRpS3A\tRpL30\nRpS3A\tRpL3\nRpS3A\tRpS3\nRpS3A\tRpS14a\nRpS3A\tRpL10Ab\nRpS3A\tRpL18A\nRpS3A\tRpS27\nRpS3A\tRpL4\nRpS3A\tRpS16\nRpS3A\tRpS18\nRpS3A\tRpS10b\nRpS16\tmago\nRpS16\tRpS14a\nRpS16\tRpS6\nRpS16\tsta\nRpS15Ab\tvig\nRpS15Ab\tCG9769\nRpS15Ab\tade3\nRpS15Ab\tEfTuM\nRpS15Ab\tSrp19\nRpS15Ab\teIF3-S9\nRpS15Ab\teIF3-S10\nRpS15Ab\teIF3-S8\nRpS15Ab\tbonsai\nRpS15Ab\tmRpS10\nRpS15Ab\tSrp54k\nRpS15Ab\tTango7\nRpS15Ab\tTctp\nRpS15Ab\tmRpS9\nRpS15Ab\tmRpL12\nRpS15Ab\tRpS15Aa\nRpS15Ab\tRpS12\nRpS15Ab\tRpL10Ab\nRpS15Ab\tRpL13\nRpS15Ab\tRpL18A\nRpS15Ab\tRpL12\nRpS15Ab\tRpL3\nRpS15Ab\tRpL4\nRpS15Ab\tRpS10a\nRpS15Ab\tRpL11\nRpS15Ab\tRpS6\nRpS15Ab\tRpS2\nRpS15Ab\tRpS13\nRpS15Ab\tRpS23\nRpS15Ab\tRpS18\nRpS15Ab\tRpS3\nRpS15Ab\tRpS10b\nRpS15Ab\tRpS17\nRpS15Ab\tRpS14a\nRpS15Ab\tsta\nRpS15Ab\tRpS11\nRpS15Ab\tRpS27\nRpS15Ab\tRpS4\nRpS15Ab\tRpS16\nRpL3\teEF1delta\nRpL3\tbonsai\nRpL3\tTcp-1zeta\nRpL3\tTcp-1eta\nRpL3\tCG7033\nRpL3\tTctp\nRpL3\tmago\nRpL3\tSrp19\nRpL3\tmRpL12\nRpL3\tmRpS9\nRpL3\teIF3-S10\nRpL3\tCG10576\nRpL3\tCG5642\nRpL3\tCG9769\nRpL3\tSrp54k\nRpL3\tTrip1\nRpL3\tade3\nRpL3\tRpS14a\nRpL3\tRpS10b\nRpL3\tRpS15Aa\nRpL3\tRpS13\nRpL3\tRpS16\nRpL3\tRpS2\nRpL3\tRpS4\nRpL3\tRpS17\nRpL3\tsta\nRpL3\tRpL12\nRpL3\tRpL13\nRpL3\tRpL10Ab\nRpL3\tRpS12\nRpL3\tRpS6\nTcp-1zeta\tTim9a\nTcp-1zeta\tRpL12\nTcp-1zeta\tHsp60\nTcp-1zeta\tsta\nTcp-1zeta\tCG7033\nT-cp1\tRpS13\nT-cp1\tRpS2\nT-cp1\tRpL13\nT-cp1\tHop\nT-cp1\tsta\nT-cp1\tRpL12\nT-cp1\tAats-ile\nT-cp1\tRpS4\nT-cp1\tRpL3\nT-cp1\tCG10576\nT-cp1\tCG7033\nT-cp1\tCctgamma\nT-cp1\tTcp-1eta\nT-cp1\tTcp-1zeta\nRpS3\tTcp-1zeta\nRpS3\tvig\nRpS3\tCG7033\nRpS3\teEF1delta\nRpS3\tade3\nRpS3\tTcp-1eta\nRpS3\tSrp72\nRpS3\tmago\nRpS3\tbonsai\nRpS3\tmRpS10\nRpS3\tmRpS9\nRpS3\tmRpL12\nRpS3\tSrp19\nRpS3\tCG5642\nRpS3\tSrp54k\nRpS3\tCG9769\nRpS3\tTctp\nRpS3\teIF3-S10\nRpS3\tRpS15Aa\nRpS3\tRpL13\nRpS3\tRpS6\nRpS3\tRpS2\nRpS3\tRpL12\nRpS3\tRpL10Ab\nRpS3\tRpS14a\nRpS3\tRpS12\nRpS3\tRpS13\nRpS3\tRpS16\nRpS3\tRpS10b\nRpS3\tsta\nRpS3\tRpS4\nRpS3\tRpL3\nRpS3\tRpS17\nRpS3\tTrip1\nRpL26\tCG10576\nRpL26\teEF1delta\nRpL26\tEfTuM\nRpL26\tNacalpha\nRpL26\tbonsai\nRpL26\tade3\nRpL26\tmRpS10\nRpL26\tmRpS5\nRpL26\tTctp\nRpL26\teIF4G\nRpL26\tCG5642\nRpL26\tmago\nRpL26\teIF3-S10\nRpL26\tRbp2\nRpL26\tmRpS9\nRpL26\teIF3-S9\nRpL26\tmRpL12\nRpL26\teIF-3p66\nRpL26\teIF3-S8\nRpL26\tSrp72\nRpL26\teIF-2gamma\nRpL26\tmRpS7\nRpL26\tCG9769\nRpL26\tElf\nRpL26\tTango7\nRpL26\tTrip1\nRpL26\tEf1beta\nRpL26\tSrp19\nRpL26\teRF1\nRpL26\tSrp54k\nRpL26\tRpS15Aa\nRpL26\tRpS15Ab\nRpL26\tRpL10Ab\nRpL26\tRpL11\nRpL26\tRpL3\nRpL26\tRpS13\nRpL26\tRpS2\nRpL26\tRpS6\nRpL26\tRpS4\nRpL26\tRpS27\nRpL26\tRpL30\nRpL26\tRpL18A\nRpL26\tRpS12\nRpL26\tRpS10a\nRpL26\tRpS18\nRpL26\tRpS3A\nRpL26\tRpS8\nRpL26\tRpS17\nRpL26\tsta\nRpL26\tRpS14a\nRpL26\tRpS11\nRpL26\tRpS23\nRpL26\tRpL13\nRpL26\tRpL12\nRpL26\tRpS10b\nRpL26\tRack1\nRpL26\tRpL4\nRpL26\tRpS3\nRpL26\tRpS7\nRpL26\tRpS16\nRpL26\tRpLP2\nRpS12\tmago\nRpS12\tRpS14a\nRpS12\tRpS15Aa\nRpS12\tRpS10b\nRpS12\tRpS6\nRpS12\tRpS16\nRpS12\tRpL12\nRpS12\tsta\nCG5642\tRpL12\nCG5642\tRpS6\nCG5642\tsta\nCG5642\tRpS10b\nCG5642\tRpS12\nCG5642\tRpS16\nCG5642\tRpS4\nCG9769\tRpS15Aa\nCG9769\tRpS17\nCG9769\tRpS12\nCG9769\tRpL12\nCG9769\tRpS10b\nCG9769\tRpL10Ab\nCG9769\tRpS4\nCG9769\tsta\nCG9769\tRpS6\nCG9769\tRpS16\nCG9769\tCG5642\nCG7033\tRpS14a\nCG7033\tsta\nCct5\tRpS13\nCct5\tade3\nCct5\tProsalpha6\nCct5\tHsc70-5\nCct5\tRpt6\nCct5\tRpS3\nCct5\tRef1\nCct5\tRpS16\nCct5\tRpL12\nCct5\tRpL13\nCct5\tCG5642\nCct5\tRpL18A\nCct5\tRpS6\nCct5\tsta\nCct5\tRpL11\nCct5\tAats-ile\nCct5\tpont\nCct5\tTrip1\nCct5\tRpS4\nCct5\tTango7\nCct5\teIF3-S9\nCct5\tRpL3\nCct5\tRpS2\nCct5\tCG10576\nCct5\tRpL4\nCct5\tHop\nCct5\tCctgamma\nCct5\tTcp-1eta\nCct5\tT-cp1\nCct5\tTcp-1zeta\nCct5\tCG7033\nRpLP2\tCG10576\nRpLP2\tbonsai\nRpLP2\tmRpS10\nRpLP2\tSrp72\nRpLP2\tSrp54k\nRpLP2\tSrp19\nRpLP2\tmago\nRpLP2\teIF3-S8\nRpLP2\teIF3-S10\nRpLP2\tCG5642\nRpLP2\tRbp2\nRpLP2\tTango7\nRpLP2\teIF3-S9\nRpLP2\teIF-3p66\nRpLP2\tNacalpha\nRpLP2\tRpS15Aa\nRpLP2\tCG9769\nRpLP2\tTrip1\nRpLP2\tTctp\nRpLP2\tRpS10a\nRpLP2\tRpS10b\nRpLP2\tRpS27\nRpLP2\tRpS3\nRpLP2\tRpS13\nRpLP2\tRpS18\nRpLP2\tRpL4\nRpLP2\tRpS7\nRpLP2\tRpL13\nRpLP2\tRpS23\nRpLP2\tRpL10Ab\nRpLP2\tRpS15Ab\nRpLP2\tRpS6\nRpLP2\tRpS14a\nRpLP2\tRpS2\nRpLP2\tRpL3\nRpLP2\tRpL12\nRpLP2\tRpL18A\nRpLP2\tRpL11\nRpLP2\tRpS11\nRpLP2\tsta\nRpLP2\tRpS16\nRpLP2\tRpS17\nRpLP2\tRpS12\nRpLP2\tRpS4\nRpS23\tCG7033\nRpS23\tvig\nRpS23\teEF1delta\nRpS23\tbonsai\nRpS23\tmRpS10\nRpS23\tSrp72\nRpS23\tmago\nRpS23\tCG5642\nRpS23\tmRpL12\nRpS23\tmRpS9\nRpS23\tTctp\nRpS23\teIF3-S9\nRpS23\teIF3-S8\nRpS23\teIF3-S10\nRpS23\tCG9769\nRpS23\tTrip1\nRpS23\tSrp19\nRpS23\tSrp54k\nRpS23\tRpS16\nRpS23\tRpS27\nRpS23\tRpL18A\nRpS23\tRpS10b\nRpS23\tRpS15Aa\nRpS23\tRpS12\nRpS23\tRpS3\nRpS23\tRpS18\nRpS23\tRpS14a\nRpS23\tRpL11\nRpS23\tRpL3\nRpS23\tRpS2\nRpS23\tsta\nRpS23\tRpS6\nRpS23\tRpL10Ab\nRpS23\tRpL12\nRpS23\tRpL13\nRpS23\tRpS17\nRpS23\tRpL4\nRpS23\tRpS4\nRpS23\tRpS10a\nRpS23\tRpS13\nRpS14a\tsta\nRack1\tvig\nRack1\tbel\nRack1\tProsalpha7\nRack1\tRpn6\nRack1\tRpn2\nRack1\teEF1delta\nRack1\tCctgamma\nRack1\tEno\nRack1\tpAbp\nRack1\tNacalpha\nRack1\teIF3-S8\nRack1\tCG10576\nRack1\tCG9769\nRack1\tade3\nRack1\tDp1\nRack1\tT-cp1\nRack1\tTcp-1zeta\nRack1\tSrp54k\nRack1\tTcp-1eta\nRack1\tSrp19\nRack1\tCG8258\nRack1\tCG7033\nRack1\tCct5\nRack1\tmRpS5\nRack1\tbonsai\nRack1\tmRpS10\nRack1\teIF3-S10\nRack1\tTctp\nRack1\teIF3-S9\nRack1\teIF-2gamma\nRack1\tTrip1\nRack1\tRpS15Ab\nRack1\tTango7\nRack1\tRpS15Aa\nRack1\tRpS10a\nRack1\tRpS14a\nRack1\tRpS10b\nRack1\tRpL10Ab\nRack1\tRpS8\nRack1\tRpL3\nRack1\tRpS11\nRack1\tRpS17\nRack1\tRpS2\nRack1\tRpS16\nRack1\tRpS13\nRack1\tRpS12\nRack1\tRpS4\nRack1\tRpS3A\nRack1\tRpS3\nRack1\tRpS18\nRack1\tRpS23\nRack1\tRpS6\nRack1\tRpL12\nRack1\tRpL13\nRack1\tRpS27\nRack1\tRpL11\nRack1\tRpL18A\nRack1\tRpL30\nRack1\tRpS7\nRack1\tRpLP2\nRack1\tRpL4\nRack1\tsta\nRpL11\tCG7033\nRpL11\tvig\nRpL11\teEF1delta\nRpL11\tTcp-1eta\nRpL11\tmago\nRpL11\teIF3-S10\nRpL11\tCG10576\nRpL11\tCG5642\nRpL11\tbonsai\nRpL11\tmRpS10\nRpL11\tSrp72\nRpL11\tmRpS9\nRpL11\tTctp\nRpL11\tmRpL12\nRpL11\tCG9769\nRpL11\tSrp19\nRpL11\tSrp54k\nRpL11\tTrip1\nRpL11\tRpS15Aa\nRpL11\tRpS13\nRpL11\tRpS18\nRpL11\tRpL3\nRpL11\tRpL12\nRpL11\tRpS10b\nRpL11\tRpS17\nRpL11\tRpL10Ab\nRpL11\tRpS10a\nRpL11\tRpL13\nRpL11\tRpS4\nRpL11\tRpS16\nRpL11\tRpS12\nRpL11\tRpS27\nRpL11\tRpS2\nRpL11\tRpS3\nRpL11\tRpS6\nRpL11\tRpS14a\nRpL11\tRpL4\nRpL11\tsta\nRpn12\tUbqn\nProsbeta7\tUba2\nProsbeta7\tCG7033\nProsbeta7\tUch-L5\nProsbeta7\tRpt4R\nProsbeta7\tRpn5\nProsbeta7\tRpt6\nProsbeta7\tRpn12\nRpS13\tCG10576\nRpS13\tCG7033\nRpS13\tmRpL12\nRpS13\tade3\nRpS13\tmago\nRpS13\tCG5642\nRpS13\teIF3-S10\nRpS13\tCG9769\nRpS13\tSrp19\nRpS13\tSrp54k\nRpS13\tTrip1\nRpS13\tRpL10Ab\nRpS13\tRpS15Aa\nRpS13\tRpS12\nRpS13\tRpS17\nRpS13\tRpL12\nRpS13\tRpS10b\nRpS13\tsta\nRpS13\tRpS16\nRpS13\tRpS4\nRpS13\tRpS6\nRpS13\tRpS14a\nRpL18A\tTcp-1zeta\nRpL18A\tvig\nRpL18A\tCG7033\nRpL18A\teEF1delta\nRpL18A\tTcp-1eta\nRpL18A\tade3\nRpL18A\tmRpS10\nRpL18A\tmago\nRpL18A\tbonsai\nRpL18A\tCG10576\nRpL18A\tCG5642\nRpL18A\teIF3-S8\nRpL18A\teIF3-S10\nRpL18A\tSrp72\nRpL18A\teIF3-S9\nRpL18A\tTctp\nRpL18A\tSrp19\nRpL18A\tCG9769\nRpL18A\tSrp54k\nRpL18A\tTrip1\nRpL18A\tRpS15Aa\nRpL18A\tRpS14a\nRpL18A\tsta\nRpL18A\tRpL10Ab\nRpL18A\tRpS2\nRpL18A\tRpL3\nRpL18A\tRpS17\nRpL18A\tRpL4\nRpL18A\tRpL13\nRpL18A\tRpS4\nRpL18A\tRpL11\nRpL18A\tRpS10a\nRpL18A\tRpS6\nRpL18A\tRpL12\nRpL18A\tRpS18\nRpL18A\tRpS16\nRpL18A\tRpS12\nRpL18A\tRpS27\nRpL18A\tRpS10b\nRpL18A\tRpS3\nRpL18A\tRpS13\nRpS4\tTcp-1zeta\nRpS4\tCG7033\nRpS4\tmago\nRpS4\tbonsai\nRpS4\tRpS15Aa\nRpS4\tRpL12\nRpS4\tsta\nRpS4\tRpS10b\nRpS4\tRpS14a\nRpS4\tRpS16\nRpS4\tRpS6\nRpS4\tRpS12\nRpL4\tvig\nRpL4\tbonsai\nRpL4\tCctgamma\nRpL4\tmRpS10\nRpL4\tTcp-1eta\nRpL4\tTctp\nRpL4\tT-cp1\nRpL4\tCG7033\nRpL4\tTcp-1zeta\nRpL4\tade3\nRpL4\tmRpS9\nRpL4\tmRpL12\nRpL4\tSrp72\nRpL4\tmago\nRpL4\tSrp19\nRpL4\tCG5642\nRpL4\tCG10576\nRpL4\tSrp54k\nRpL4\tCG9769\nRpL4\teIF3-S10\nRpL4\tRpS15Aa\nRpL4\tRpS10a\nRpL4\tRpL10Ab\nRpL4\tTrip1\nRpL4\tRpS16\nRpL4\tRpS6\nRpL4\tRpS12\nRpL4\tRpS14a\nRpL4\tRpS2\nRpL4\tRpL12\nRpL4\tRpS27\nRpL4\tsta\nRpL4\tRpS13\nRpL4\tRpS10b\nRpL4\tRpS3\nRpL4\tRpS17\nRpL4\tRpL13\nRpL4\tRpL3\nRpL4\tRpS4",
      },
      {
        kind: 'annotation',
        name: 'Tau Louvain communities',
        fileName: 'TAU_Louvain.txt',
        text: "Group-1\tMgstl,KP78a,CG9257,par-1,CG31064,LpR1,Rab39,LpR2,CG3529,Lam,LamC,Sap-r,Vps60,lqf,ninaC,Hrs,Arr2,Arr1,Rab5,CG32683,pins,chp\nGroup-2\tSc2,Uch,Prx2540-1,Ace,CG4572,Sgt,CG14715,CG10359,Ect3,CalpB,Prx5,Ost48,Hsp60C,Prx2540-2,Hsp60,Ubqn,Hsp70Ab,Ugt,Iswi,Trap1,Cat,Hsc70-5,CG8209,DnaJ-H,p47,Rad23,Cnx99A,pont,Cp1,cathD,Uba2,Pfk,Gdi,Uch-L5,Rpt4R,Prosalpha1,mor,Bap55,Ufd1-like,TER94,Hsc70-3,PyK,Hsp83,ERp60,arm,Rpt1,Rpn13,Rpt2,Rpn5,Rpt6R,Rpn2,Pdi,CaBP1,Gp93,Rpt4,Hop,CG1416,Prosalpha6,Rpt5,Prosalpha7,Rpn6,CG8258,Rpt6,Rpn3,Rpn12,Prosbeta7,Usp5,CG2918,CG2852,CG1354,CG6891,HDAC6,Hsp27,His1,CG11577\nGroup-3\tshep,CG5590,CG17597,ScpX,Mfe2\nGroup-4\tCG4882,EfTuM,Rbp2,eEF1delta,CG10576,Nacalpha,mRpS34,vig,CG4679,mRpS35,mRpS7,mRpL44,mRpS22,eIF4G,bonsai,mRpS10,Srp54k,mRpL12,Tctp,eIF-2gamma,mRpS23,mRpS18B,ade3,mRpS9,mRpS5,Srp19,Srp72,eIF-3p66,Cctgamma,Ef1beta,Tango7,RpL10Ab,Trip1,eIF3-S10,eIF3-S9,Tcp-1eta,RpL13,RpS11,eIF3-S8,RpS6,RpS2,RpL12,RpS7,RpL10,RpS8,RpL30,RpS10a,RpS17,RpLP0,eRF1,Elf,RpS15Aa,RpS27,RpS18,RpS10b,RpS3A,RpS16,RpS15Ab,RpL3,Tcp-1zeta,T-cp1,RpS3,RpL26,RpS12,CG5642,CG9769,CG7033,Cct5,RpLP2,RpS23,RpS14a,Rack1,RpL11,RpS13,RpL18A,RpS4,RpL4,sta,wuho,bsf,kra,Hsp67Bc,su(r),FeCH,CG8635\nGroup-5\tnito,bol,Kap-alpha3,CG2082,lig,clu,Dbp80,Plc21C,Hrb27C,bel,Dp1,Lpin,Pkc53E,Nlp,rin,poe,CG43367,emb,fne,RanBPM,yps,Art4,CG10077,mub,AGO1,glo,Srp54,Rbp9,Rae1,Mtor,Ref1,mago,B52,LS2,U2af50,Fmr1,Top2,heph,Hrb98DE,Tudor-SN,SF2,me31B,CG16941,Prp8,Tango4,U2A,smt3,pAbp,Not1,Not3,CG2091,Capr,Gbeta76C,CG10777,CG6617,CG3800,tyf,CG11505\nGroup-6\tCG6178,frj,CG9090,Ssadh,l(1)G0156,CG32026,Men-b,CG11899,CG16935,AGBE,ImpL3,Nc73EF,CG3902,CG11876,Pdk,Vha100-1,AcCoAS,ACC,Mdh2,Cyt-c-p,Aldh,CG15093,ND-51L1,ND-51,Vha68-2,Vha100-2,ND-15,CG12262,yip2,ade5,CG11089,COX6B,CG11752,COX4,COX5A,Eno,Tpi,ND-PDSW,NP15.6,ATPsynbeta,ATPsynCF6,CG10932,CG33791,CG3011,Men,sun,porin,ND-B16.6,CG7603,Mpcp\nGroup-7\tNdg,trol,Cg25C,vkg,LanA,LanB2,Glt\nGroup-8\tLar,CG31715,PlexA,gammaSnap2,CG10186,Klp10A,Mtl,bai,Act79B,Abl,Tim17b,p115,Sec24AB,robl,tsr,flr,FKBP59,bic,BicD,CadN,syd,Rac1,zip,alphaCOP,zetaCOP,CG7048,CG7770,Dlic,Dhc64C,betaCOP,cpa,cpb,epsilonCOP,beta'COP,Klc,Khc,alpha-Cat,deltaCOP,RhoGDI,Dscam1,Tim9a,sqh,dock,Dscam2,CG8498,CLIP-190,betaTub56D\nGroup-9\tMapmodulin,Aats-val,Aats-his,Aats-asp,CG33123,Aats-ile,Aats-arg,CG31739\n",
      },
      {
        kind: 'annotation',
        name: 'Tau KEGG pathways',
        fileName: 'TAU_KEGG_Annotation_NORMA.txt',
        text: 'Ribosome\tRpL10,RpL10Ab,RpL11,RpL12,RpL13,RpL18A,RpL26,RpL3,RpL30,RpL4,RpLP0,RpLP2,RpS10a,RpS10b,RpS11,RpS12,RpS13,RpS15Aa,RpS15Ab,RpS16,RpS17,RpS18,RpS2,RpS23,RpS27,RpS3,RpS3A,RpS4,RpS6,RpS7,RpS8,bonsai,mRpL12,mRpS10,mRpS7,mRpS9,sta\nProteasome\tProsalpha6,Prosalpha7,Prosbeta7,Rpn12,Rpn13,Rpn2,Rpn3,Rpn5,Rpn6,Rpt1,Rpt2,Rpt4,Rpt4R,Rpt5,Rpt6,Rpt6R\nSpliceosome\tB52,CG10077,CG10777,CG16941,Hrb98DE,LS2,Prp8,Ref1,SF2,Tango4,U2A,U2af50,mago\nPhagosome\tAct79B,Cnx99A,Cp1,Dhc64C,Dlic,Hrs,Mtl,Rab5,Rac1,Vha100-1,Vha100-2,Vha68-2,betaTub56D\nCarbon metabolism\tAcCoAS,CG10932,CG11876,CG11899,CG12262,CG3011,CG32026,CG33791,Cat,Eno,Mdh2,Men,Men-b,Nc73EF,Pfk,PyK,Tpi,l(1)G0156\nOxidative phosphorylation\tATPsynCF6,ATPsynbeta,COX4,COX5A,COX6B,ND-15,ND-51,ND-51L1,ND-B16.6,ND-PDSW,NP15.6,Vha100-1,Vha100-2,Vha68-2,sun\nRNA degradation\tCG2091,Eno,Hsc70-5,Hsp60,Hsp60C,Not1,Not3,Pfk,me31B,pAbp\nRNA transport\tCG9769,Fmr1,Mtor,Rae1,Ref1,Trip1,eIF-3p66,eIF3-S10,eIF3-S8,eIF3-S9,eIF4G,emb,mago,pAbp,smt3\nGlycolysis\tAcCoAS,Aldh,CG11876,Eno,ImpL3,Pfk,PyK,Tpi\nPyruvate metabolism\tACC,AcCoAS,Aldh,CG10932,CG11876,ImpL3,Mdh2,Men,Men-b,PyK\n',
      },
      {
        kind: 'colors',
        name: 'Tau expression',
        fileName: 'TAU_expressions.txt',
        text: "CG7603\tred\nHis1\tred\nAct79B\tred\nGbeta76C\tred\nCG11876\tred\nKap-alpha3\tred\nCG2082\tred\nLanA\tred\nVha68-2\tred\nCalpB\tred\nSsadh\tred\nCG43367\tred\nl(1)G0156\tred\nCG9090\tred\nPrx5\tred\nCG1354\tred\nporin\tred\nGstD9\tred\nCG32026\tred\nAldh\tred\nGlt\tred\nTpi\tred\nPyK\tred\nATPsynbeta\tred\nEno\tred\nMtl\tred\nArr1\tred\nMpcp\tred\nArr2\tred\nNdg\tred\nFeCH\tred\nNc73EF\tred\nCG10932\tred\nCG33791\tred\nbetaTub56D\tred\nCyt-c-p\tred\nCG16935\tred\nPrx2540-2\tred\nGstE4\tred\npins\tred\nchp\tred\nMen-b\tred\nGstD3\tred\nHDAC6\tred\nCG10359\tred\nmago\tred\nLamC\tred\nAce\tred\nMdh2\tred\nninaC\tred\nLanB2\tred\nCg25C\tred\nsun\tred\nvkg\tred\nPfk\tred\nPrx2540-1\tred\nEfTuM\tred\nVha100-1\tred\nVha100-2\tred\nAGBE\tred\nfrj\tred\nDnaJ-H\tred\ntrol\tred\nMen\tgreen\np47\tgreen\nmRpS10\tgreen\nNP15.6\tgreen\nRpS15Ab\tgreen\nNot3\tgreen\nTim17b\tgreen\neIF4G\tgreen\ncathD\tgreen\nCG12262\tgreen\nProsalpha7\tgreen\nUsp5\tgreen\nyip2\tgreen\nDscam1\tgreen\nCG2918\tgreen\ncpa\tgreen\nRpS4\tgreen\nade3\tgreen\nCG8209\tgreen\nCG11505\tgreen\nFKBP59\tgreen\nRpS11\tgreen\nRab39\tgreen\nRpL13\tgreen\nCG1371\tgreen\nPrp8\tgreen\nProsalpha6\tgreen\nRpn12\tgreen\nRpn2\tgreen\nRpn5\tgreen\nNacalpha\tgreen\nHsp60\tgreen\nTudor-SN\tgreen\nRpt5\tgreen\nRpt4\tgreen\nlqf\tgreen\nRpt1\tgreen\nTop2\tgreen\nRpn6\tgreen\nND-PDSW\tgreen\nsta\tgreen\nsqh\tgreen\nCG6178\tgreen\nCG32683\tgreen\nAats-asp\tgreen\nLpR2\tgreen\neIF-2gamma\tgreen\nRpL11\tgreen\nElf\tgreen\nERp60\tgreen\nSec24AB\tgreen\nCG11089\tgreen\nRae1\tgreen\nCG3011\tgreen\nAbl\tgreen\nRpt6R\tgreen\nUch\tgreen\nTrap1\tgreen\nCG7048\tgreen\nRpt6\tgreen\nCG10186\tgreen\nwuho\tgreen\nAats-arg\tgreen\nLam\tgreen\nHsp60C\tgreen\nTrip1\tgreen\nRpS3\tgreen\nTctp\tgreen\nCG31064\tgreen\nDscam2\tgreen\nemb\tgreen\nUba2\tgreen\npAbp\tgreen\nCapr\tgreen\nCG4972\tgreen\nAats-his\tgreen\nAats-ile\tgreen\nPlexA\tgreen\nCctgamma\tgreen\nRab5\tgreen\nzip\tgreen\nMi-2\tgreen\nAcCoAS\tgreen\nsu(r)\tgreen\nmRpS23\tgreen\narm\tgreen\nCG10077\tgreen\nRef1\tgreen\nRpS7\tgreen\nCG10777\tgreen\nAats-val\tgreen\nImpL3\tgreen\nPdi\tgreen\nSgt\tgreen\nbeta'COP\tgreen\nalphaCOP\tgreen\nFmr1\tgreen\nCG31715\tgreen\nmRpS7\tgreen\nEf1beta\tgreen\nND-B16.6\tgreen\nScpX\tgreen\nCat\tgreen\nlig\tgreen\nCG10576\tgreen\np115\tgreen\nade5\tgreen\nBap55\tgreen\nHsp83\tgreen\nRpL4\tgreen\nProsbeta7\tgreen\nRpLP2\tgreen\nCG4572\tgreen\nCG31739\tgreen\nTim9a\tgreen\npont\tgreen\nHsc70-5\tgreen\nHsp27\tgreen\nbic\tgreen\nHsp67Bc\tgreen\nKP78a\tgreen\nCG3529\tgreen\nBicD\tgreen\nRpS12\tgreen\nbsf\tgreen\nHsc70-3\tgreen\nHrb98DE\tgreen\nRpS8\tgreen\nCG8498\tgreen\nDp1\tgreen\nOst48\tgreen\nSrp54k\tgreen\nRad23\tgreen\nTango4\tgreen\nkra\tgreen\nHsp70Ab\tgreen\nRpS15Aa\tgreen\ntyf\tgreen\nLpin\tgreen\nCnx99A\tgreen\nmRpS18B\tgreen\neEF1delta\tgreen\nCG3800\tgreen\nRpL18A\tgreen\npar-1\tgreen\nme31B\tgreen\nCOX4\tgreen\neIF3-S10\tgreen\nCG11752\tgreen\nCG14715\tgreen\nMtor\tgreen\nU2af50\tgreen\nmRpS5\tgreen\nKhc\tgreen\nCct5\tgreen\nRpL12\tgreen\nyps\tgreen\nB52\tgreen\nCG1416\tgreen\nbonsai\tgreen\nTER94\tgreen\nLar\tgreen\nalpha-Cat\tgreen\nglo\tgreen\nCadN\tgreen\nCG7033\tgreen\nCG2091\tgreen\nRpS17\tgreen\nArt4\tgreen\nsyd\tgreen\nRpL3\tgreen\nTcp-1eta\tgreen\nCG4679\tgreen\nSrp54\tgreen\nmRpS34\tgreen\nvig\tgreen\nSrp72\tgreen\nND-15\tgreen\nMapmodulin\tgreen\nRack1\tgreen\nCp1\tgreen\nDlic\tgreen\nRpS18\tgreen\nfne\tgreen\nRpS14a\tgreen\neIF3-S8\tgreen\nCG5642\tgreen\nRpLP0\tgreen\ngammaSnap2\tgreen\nrobl\tgreen\nRpS16\tgreen\nUgt\tgreen\nHrs\tgreen\nKlc\tgreen\neIF-3p66\tgreen\nND-51L1\tgreen\nbetaCOP\tgreen\nzetaCOP\tgreen\nUbqn\tgreen\nrin\tgreen\neRF1\tgreen\nLS2\tgreen\nDbp80\tgreen\nUfd1-like\tgreen\neIF3-S9\tgreen\nND-51\tgreen\nCOX6B\tgreen\nmRpS9\tgreen\nCG33123\tgreen\nCG9769\tgreen\nbel\tgreen\nSap-r\tgreen\ndeltaCOP\tgreen\nRbp9\tgreen\nRpS13\tgreen\nCG9257\tgreen\nmRpL12\tgreen\nMfe2\tgreen\nCaBP1\tgreen\nAGO1\tgreen\nMgstl\tgreen\nCG17597\tgreen\nCG6617\tgreen\nmub\tgreen\nATPsynCF6\tgreen\nRbp2\tgreen\nUch-L5\tgreen\ndock\tgreen\nLpR1\tgreen\nCG6891\tgreen\nRpL10\tgreen\nheph\tgreen\nHop\tgreen\nRpL10Ab\tgreen\nCOX5A\tgreen\nRpL30\tgreen\nmRpS22\tgreen\nTcp-1zeta\tgreen\nNlp\tgreen\nRpt4R\tgreen\nEct3\tgreen\npoe\tgreen\nVps60\tgreen\ntsr\tgreen\nRanBPM\tgreen\nPlc21C\tgreen\nmor\tgreen\nepsilonCOP\tgreen\nRpt2\tgreen\nACC\tgreen\nRpn3\tgreen\nbol\tgreen\nRpS10a\tgreen\nCG4882\tgreen\nCG8258\tgreen\nbai\tgreen\nshep\tgreen\nSrp19\tgreen\nCG2852\tgreen\nCG11899\tgreen\nclu\tgreen\nIswi\tgreen\nDhc64C\tgreen\nRpS6\tgreen\nRpS10b\tgreen\nCG5590\tgreen\nKlp10A\tgreen\nGdi\tgreen\nRpS2\tgreen\nRpS27\tgreen\nNot1\tgreen\nflr\tgreen\nPkc53E\tgreen\nCG11577\tgreen\nCG15093\tgreen\nCLIP-190\tgreen\nsmt3\tgreen\nRac1\tgreen\ncpb\tgreen\nCG16941\tgreen\nRhoGDI\tgreen\nProsalpha1\tgreen\nmRpL44\tgreen\nGp93\tgreen\nPdk\tgreen\nRpS23\tgreen\nT-cp1\tgreen\nRpL26\tgreen\nCG3902\tgreen\nCG7770\tgreen\nmRpS35\tgreen\nCG8635\tgreen\nRpS3A\tgreen\nTango7\tgreen\nHrb27C\tgreen\nnito\tgreen\nSc2\tgreen\nU2A\tgreen\nRpn13\tgreen\nSF2\tgreen\n",
      },
    ],
  },
  coexpr: {
    title: 'Human gene co-expression',
    source: 'PMID 19081792, PMCID PMC2597745',
    link: 'https://doi.org/10.1371/journal.pone.0003911',
    files: [
      {
        kind: 'network',
        name: 'Human co-expression',
        fileName: 'NORMA_Human_coexpression_NETWORK.txt',
        text: 'Source\tTarget\nRPL24\tRPL41\nRPL24\tRPL26\nRPL24\tRPL30\nRPL24\tTPT1\nRPL24\tRPL9\nRPL24\tRPS25\nRPL24\tRPS23\nRPL24\tRPL31\nRPL24\tRPS27A\nRPL24\tU16\nRPL24\tRPS4X\nRPL24\tRPL7\nRPL24\tRPL4\nRPL24\tRPL5\nRPL24\tRPS3A\nRPL24\tRPS7\nRPL24\tRPL34\nRPL24\tRPL35A\nRPL24\tRPS24\nRPL24\tRPL6\nRPL41\tRPL5\nRPL41\tRPS3A\nRPL41\tTPT1\nRPL41\tRPS23\nRPL41\tRPS4X\nHNRPA1\tRPS25\nHNRPA1\tRPS24\nHNRPA1\tRPS4X\nHNRPA1\tRPS6\nHNRPA1\tRPL22\nHNRPA1\tNCL\nHNRPA1\tRPL24\nHNRPA1\tNPM1\nHNRPA1\tRPL4\nHNRPA1\tMDS1\nHNRPA1\tU16\nHNRPA1\tRPS23\nHNRPA1\tRPS7\nHNRPA1\tSF3B1\nHNRPA1\tNAP1L1\nHNRPA1\tRPL17\nHNRPA1\tRPL11\nHNRPA1\tRPL6\nHNRPA1\tRPL34\nHNRPA1\tLOC440055\nHNRPA1\tRPS3A\nHNRPA1\tRPL7\nRPS25\tRPS4X\nRPS25\tRPS3A\nRPS25\tRPS6\nRPS25\tU16\nRPS25\tRPS7\nRPS25\tRPS27A\nH3F3A\tRPA1\nH3F3A\tRAP1B\nH3F3A\tRPLP2\nH3F3A\tRPS20\nH3F3A\tRPL27\nH3F3A\tRPS11\nH3F3A\tRPL27A\nCOX8A\tCYC1\nCOX8A\tNDUFS8\nCOX8A\tSLC25A11\nCOX8A\tSLC25A3\nCOX8A\tNDUFAB1\nCOX8A\tNDUFB11\nCOX8A\tMRPS12\nCYC1\tETFB\nCYC1\tUQCRC1\nATP8B1\tPDE4C\nATP8B1\tFLJ42393\nATP8B1\tDKFZP566N034\nATP8B1\tCEP27\nATP8B1\tKNS2\nATP8B1\tRIOK3\nATP8B1\tDBT\nATP8B1\tPGF\nATP8B1\tZNF611\nATP8B1\tSLC35E1\nATP8B1\tLOC152719\nATP8B1\tFBXW12\nATP8B1\tZNF160\nPDE4C\tSPG21\nPDE4C\tUBE2D2\nPDE4C\tPRKAR2A\nPDE4C\tZNF611\nPDE4C\tPRR11\nPDE4C\tPGF\nPDE4C\tZNF160\nPDE4C\tRIOK3\nPDE4C\tSLC35E1\nLOC58486\tSUB1\nLOC58486\tMGEA5\nLOC58486\tRNPC2\nLOC58486\tPFAAP5\nLOC58486\tWAC\nLOC58486\tMATR3\nLOC58486\tPNRC2\nLOC58486\tPAPOLA\nLOC58486\tZNF638\nLOC58486\tSF3B1\nLOC58486\tUSP34\nLOC402057\tRPS7\nLOC402057\tRPS15A\nLOC402057\tRPL34\nLOC402057\tRPS4X\nLOC402057\tRPS6\nLOC402057\tRPL31\nLOC402057\tRPL35A\nLOC402057\tRPL37\nLOC402057\tRPL22\nLOC402057\tRPL4\nLOC402057\tRPL9\nLOC402057\tRPS23\nLOC402057\tRPL11\nLOC402057\tLOC440055\nLOC402057\tRPL30\nLOC402057\tRPL23\nLOC402057\tRPL36A\nLOC402057\tRPL39\nLOC402057\tMDS1\nLOC402057\tRPL32\nLOC402057\tRPS17\nRPS7\tSFRS5\nRPS7\tSF3B1\nRPS7\tU16\nMDS1\tRPL23\nMDS1\tRNPC2\nMDS1\tRPL17\nMDS1\tRPL5\nMDS1\tRPS3A\nMDS1\tRPL41\nMDS1\tRPL4\nMDS1\tTPT1\nMDS1\tNPM1\nMDS1\tRPS6\nMDS1\tRPL7\nMDS1\tRPL24\nMDS1\tRPL34\nMDS1\tRPL26\nMDS1\tRPL22\nMDS1\tRPS27A\nMDS1\tRPL6\nMDS1\tRPS25\nMDS1\tRPS15A\nMDS1\tU16\nMDS1\tRPS7\nMDS1\tRPS24\nMDS1\tRPL36A\nMDS1\tRPS4X\nMDS1\tRPL21\nMDS1\tRPL31\nMDS1\tRPL35A\nMDS1\tRPL30\nMDS1\tRPS17\nMDS1\tRPL9\nMDS1\tRPL11\nMDS1\tRPS23\nRPL23\tRPL7\nRPL23\tRPL26\nRPL23\tRPL5\nRPL23\tRPL24\nRPL23\tRPS27\nRPL23\tRPS15A\nRPL23\tRPS27A\nRPL23\tRPL6\nRPL23\tRPL36A\nRPL23\tU16\nRPL23\tRPS25\nRPL23\tRPS17\nRPL23\tRPS4X\nRPL23\tRPS7\nRPL23\tRPS3A\nRPL23\tRPS23\nRPL23\tRPL30\nRPL23\tRPS24\nRPL23\tRPL9\nRPL23\tRPL31\nRPL23\tRPS6\nRPL23\tRPL35A\nRPL23\tRPL4\nRPL23\tRPL34\nRPL5\tRPS6\nRPL5\tRPS23\nRPL5\tTPT1\nRPL5\tRPS25\nRPL5\tRPS3A\nRPL5\tRPS4X\nRPL5\tRPL9\nRPL5\tRPS27A\nRPL5\tRPL7\nRPL5\tRPS7\nRPL5\tU16\nRPL5\tRPS24\nRPL5\tRPL6\nATP5G3\tCOX8A\nATP5G3\tNDUFB11\nATP5G3\tSLC25A3\nATP5G3\tUQCRC1\nATP5G3\tUQCRQ\nATP5G3\tCOX5A\nATP5G3\tCOX4I1\nATP5G3\tNDUFAB1\nATP5G3\tUQCRFS1\nATP5G3\tCOX5B\nATP5G3\tMDH2\nCOPB\tTRAM1\nCOPB\tLOC58486\nCOPB\tRNPC2\nCOPB\tHSP90B1\nCOPB\tYME1L1\nCOPB\tPAPOLA\nPRKACB\tSEPT7\nSEPT7\tSH3BGRL\nSEPT7\tZFR\nUBE2D2\tZNF160\nRNPC2\tTAF7\nRNPC2\tTCEA1\nRNPC2\tZNF638\nRNPC2\tRPS3A\nRNPC2\tSF3B1\nRNPC2\tRPL5\nRNPC2\tRPL6\nRNPC2\tRPL17\nRNPC2\tSRP72\nIGHM\tIGKC\nIGHM\tIGLC2\nIGHM\tIGKV1-5\nIGHM\tLOC91316\nIGHM\tIGLV3-10\nIGHM\tIGL@\nIGHM\tIGHV1-69\nIGHM\tIGKV1D-13\nIGHM\tIGLV3-25\nIGHM\tIGLC1\nIGHM\tIGKV1OR2-108\nIGKC\tIGLJ3\nIGKC\tIGLC2\nIGKC\tIGKV1D-13\nIGKC\tIGLV3-25\nIGKC\tIGKV1OR2-108\nIGKC\tIGKV1-5\nIGKC\tIGLV3-10\nIGKC\tIGL@\nIGKC\tLOC91316\nIGKC\tIGLC1\nIGKC\tIGKV1OR15-118\nLOC342346\tYTHDC1\nLOC342346\tXPO1\nLOC342346\tSON\nLOC342346\tTAF7\nLOC342346\tPAPOLA\nLOC342346\tSH3BGRL\nEEF1A1\tRPL17\nEEF1A1\tMDS1\nEEF1A1\tRPS24\nEEF1A1\tRPS3A\nEEF1A1\tRPL22\nEEF1A1\tNPM1\nEEF1A1\tRPL11\nEEF1A1\tRPS7\nEEF1A1\tRPS27A\nEEF1A1\tTPT1\nEEF1A1\tRPS6\nEEF1A1\tRPL7\nEEF1A1\tRPS4X\nEEF1A1\tRPL23\nRPL17\tSFRS5\nRPL17\tRPS25\nRPL17\tRPL9\nRPL17\tTPT1\nRPL17\tRPL30\nRPL17\tRPL26\nRPL17\tRPL7\nRPL17\tRPL36A\nRPL17\tRPL22\nRPL17\tRPS6\nRPL17\tSF3B1\nRPL17\tRPS4X\nRPL17\tTINP1\nRPL17\tRPL35A\nRPL17\tRPS23\nRPL17\tU16\nRPL17\tRPS3A\nRPL17\tRPL24\nRPL17\tRPS7\nRPL17\tRPL23\nRPL17\tRPL31\nRPL17\tRPS27A\nRPL17\tRPL21\nRPL17\tRPL4\nRPL17\tRPL5\nRPL17\tRPL34\nRPL17\tRPL6\nRPL17\tRPS24\nRPL13A\tRPS9\nRPL13A\tRPS18\nRPL13A\tRPL18A\nRPL13A\tRPL27A\nRPL13A\tRPL14\nRPL13A\tRPS16\nRPL13A\tRPS14\nRPL13A\tRPL15\nRPL13A\tRPLP2\nRPL13A\tRPS21\nRPL13A\tRPL29\nRPL13A\tRPS19\nRPL13A\tRPL36\nRPL13A\tRPS5\nNDUFAB1\tNDUFB4\nNDUFAB1\tNDUFS3\nNDUFAB1\tUQCRC1\nNDUFAB1\tNDUFV1\nNDUFAB1\tUQCR\nNDUFAB1\tNDUFB2\nNDUFAB1\tUQCRQ\nNDUFAB1\tNDUFS7\nNDUFAB1\tNDUFB11\nNDUFAB1\tNDUFC1\nNDUFAB1\tPOLR2I\nNDUFAB1\tNDUFS8\nNDUFAB1\tUQCRFS1\nNDUFB4\tNDUFS3\nNDUFB4\tNDUFS8\nNDUFB4\tUQCRFS1\nNDUFB4\tUCRC\nAKR7A2\tCLPP\nAKR7A2\tTIMM13\nAKR7A2\tNDUFV1\nAKR7A2\tMRPL34\nAKR7A2\tC21orf33\nAKR7A2\tNDUFS8\nCLPP\tTUFM\nCLPP\tMRPL34\nhfl-B5\tHNRPA2B1\nhfl-B5\tHIATL1\nhfl-B5\tHNRPH1\nhfl-B5\tHNRPA1\nHNRPA2B1\tXPO1\nHNRPA2B1\tSRP46\nSET\tSFRS3\nSET\tSRP46\nSET\tSON\nSET\tSFRS10\nHLA-C\tISG20\nHLA-C\tMAN2B1\nHLA-C\tHLA-G\nHLA-C\tHLA-F\nHLA-C\tHLA-E\nHLA-C\tTAPBP\nHLA-C\tHLA-J\nISG20\tRAC2\nISG20\tPSMB9\nISG20\tUCP2\nRPS24\tTPT1\nRPS24\tTCEA1\nRPS24\tSF3B1\nRPS24\tRPS25\nRPS24\tRPS3A\nRPS24\tRPS6\nRPS24\tRPS4X\nRPS24\tU16\nRPS24\tRPS27A\nRPS24\tRPS7\nDDX1\tKTN1\nKTN1\tUSP47\nRPS4X\tU16\nRPS4X\tTPT1\nRPS4X\tRPS7\nRPS4X\tRPS6\nCCNL1\tRPL4\nCCNL1\tZNF638\nCCNL1\tNACA\nCCNL1\tRPL5\nCCNL1\tRPS27A\nCCNL1\tRPL17\nCCNL1\tRNPC2\nCCNL1\tRPL21\nCCNL1\tRPL6\nCCNL1\tEIF3S6\nRPL4\tTPT1\nRPL4\tRPL7\nRPL4\tSFRS5\nRPL4\tRPS15A\nRPL4\tRPL5\nRPL4\tRPS17\nRPL4\tRPS25\nRPL4\tRPL9\nRPL4\tRPS7\nRPL4\tRPS27A\nRPL4\tRPL6\nRPL4\tRPS23\nRPL4\tRPS4X\nRPL4\tRPS3A\nRPL4\tRPS24\nRPL4\tU16\nRPL4\tRPS6\nFLJ11021\tNAP1L1\nFLJ11021\tZNF403\nFLJ11021\tHIATL1\nFLJ11021\tKIAA0907\nFLJ11021\tWAC\nFLJ11021\tTUG1\nFLJ11021\tPAPOLA\nFLJ11021\tRNPC2\nFLJ11021\tXPO1\nFLJ11021\tLOC58486\nFLJ11021\tNPM1\nNAP1L1\tRPL17\nNAP1L1\tYTHDC1\nNAP1L1\tPAPOLA\nNAP1L1\tZNF9\nNAP1L1\tXPO1\nC21orf33\tETFB\nC21orf33\tCOX5B\nC21orf33\tNDUFS7\nC21orf33\tNDUFS8\nETFB\tNDUFS8\nETFB\tUQCRQ\nCOX5A\tNDUFS8\nCOX5A\tNDUFB11\nCOX5A\tNDUFS7\nCOX5A\tMDH2\nCOX5A\tCOX5B\nCOX5A\tNDUFAB1\nCOX5A\tUQCRFS1\nCOX5A\tUQCRC1\nNDUFS8\tSLC25A3\nNDUFS8\tPOLR2I\nNDUFS8\tPOLR2L\nNDUFS8\tTIMM13\nNDUFS8\tUQCRFS1\nNDUFS8\tTMEM93\nNDUFS8\tUQCRQ\nCROP\tIL6ST\nCROP\tZNF638\nCROP\tHNRPA3P1\nCROP\tRNPC2\nCROP\tMATR3\nCROP\tSFRS5\nCROP\tXPO1\nCROP\tSFPQ\nCROP\tLOC58486\nCROP\tHNRPDL\nCROP\tTUG1\nCROP\tSFRS11\nCROP\tZC3H11A\nCROP\tPFAAP5\nCROP\tNPM1\nCROP\tFLJ10154\nCROP\tGOLGA8A\nIL6ST\tSFRS11\nPOLR2E\tSIAHBP1\nRPL26\tRPS3A\nRPL26\tRPL4\nRPL26\tRPL7\nRPL26\tRPS7\nRPL26\tRPS6\nRPL26\tRPS27A\nRPL26\tRPL34\nRPL26\tRPS24\nRPL26\tRPS25\nRPL26\tRPL5\nRPL26\tRPL6\n216858_x_at\tCEP27\n216858_x_at\tZNF611\n216858_x_at\tFLJ14346\n216858_x_at\tLOC152719\nCEP27\tPRR11\nCEP27\tPGF\nCEP27\tPRKAR2A\nCEP27\tLOC152719\nCEP27\tPDE4C\nCEP27\tRIOK3\nCEP27\tZNF611\nCEP27\tKNS2\nCEP27\tFBXW12\nCEP27\tDBT\nCEP27\tZNF160\nCEP27\tSLC35E1\nCEP27\tFLJ12151\nRPL6\tRPL9\nRPL6\tSFRS5\nRPL6\tTPT1\nRPL6\tRPL7\nRPL6\tRPS23\nRPL6\tSF3B1\nRPL6\tRPS4X\nRPL6\tRPS25\nRPL6\tRPS3A\nRPL6\tRPS6\nRPL6\tU16\nRPL6\tRPS7\nRPL6\tRPS27A\nRPL6\tRPS24\nRPL9\tRPS27A\nRPL9\tRPS24\nRPL9\tU16\nRPL9\tRPS25\nRPL9\tRPS4X\nRPL9\tRPS23\nRPL9\tRPS7\nRPL9\tRPS3A\nRPL9\tRPS17\nRPL9\tRPS6\nRPL9\tRPS27\nRPL9\tRPS15A\nDDX3X\tXPO1\nDDX3X\tNAP1L1\nDDX3X\tPAPOLA\nDDX3X\tSON\nDDX3X\tHNRPR\nDDX3X\tTHRAP1\nDDX3X\tSH3BGRL\nDDX3X\tHNRPH1\nDDX3X\tMGEA5\nDDX3X\tHIATL1\nDDX3X\tDDX5\nDDX3X\tLOC342346\nDDX3X\tEIF4G2\nDDX3X\tSF3B1\nDDX3X\tYTHDC1\nCOPE\tSCAND1\nCOPE\tMRPS12\nCOPE\tNAPA\nCOPE\tFIBP\nSCAND1\tTHRAP5\nCOPS2\tRPL15\nCOPS2\tHSP90B1\nCOPS2\tHNRPH3\nCOPS2\tZNF638\nCOPS2\tKIDINS220\nCOPS2\tVDP\nCOPS2\tTLOC1\nCOPS2\tCSDE1\nRPL15\tRPL35\nRPL15\tRPS16\nITGB2\tRAC2\nITGB2\tRGS19\nRAC2\tTRA@\nRAC2\tTRB@\nDBT\tPRR11\nDBT\tDIP2A\nDBT\tFLJ42393\nDBT\tZNF611\nDBT\tFLJ12151\nDBT\tPGF\nDBT\tLOC152719\nDBT\tRIOK3\nDBT\tPDE4C\nDBT\tWDR45\nDBT\tZNF160\nDBT\tFBXW12\nDBT\tKNS2\nDBT\tSLC35E1\nPRR11\tZNF611\nPRR11\tSLC35E1\nPRR11\tZNF160\n207730_x_at\t216858_x_at\n207730_x_at\tUBE2D2\n207730_x_at\tFLJ42393\n207730_x_at\t208246_x_at\n207730_x_at\tPRKAR2A\n207730_x_at\t217679_x_at\n207730_x_at\tPDE4C\n207730_x_at\tPRR11\n207730_x_at\tPGF\n207730_x_at\tRIOK3\n207730_x_at\t208238_x_at\n207730_x_at\tKNS2\n207730_x_at\t216524_x_at\n207730_x_at\tATP8B1\n207730_x_at\tFLJ12151\n207730_x_at\tLOC152719\n207730_x_at\tDBT\n207730_x_at\tZNF611\n207730_x_at\tSLC35E1\n207730_x_at\tZNF160\n207730_x_at\tCEP27\n207730_x_at\tFBXW12\nEEF1D\tRPS9\nEEF1D\tGLTSCR2\nEEF1D\tRPL38\nPRDX2\tRIOK3\nRIOK3\tZNF611\nRIOK3\tZNF160\nRIOK3\tSLC35E1\nRPS23\tRPS27A\nRPS23\tU16\nRPS23\tRPS7\nRPS23\tRPS24\nRPS23\tRPS25\nRPS23\tRPS6\nRPS23\tRPS4X\nRPS23\tRPS3A\nRPS27A\tSFRS5\nRPS27A\tRPS4X\nRPS27A\tU16\nRPS27A\tTINP1\nRPS27A\tRPS3A\nRPS27A\tRPS7\nRPS27A\tRPS6\nRPL34\tTPT1\nRPL34\tSF3B1\nRPL34\tRPL7\nRPL34\tRPS25\nRPL34\tRPL36A\nRPL34\tRPL5\nRPL34\tRPS27A\nRPL34\tRPS4X\nRPL34\tRPS3A\nRPL34\tRPS6\nRPL34\tRPS7\nRPL34\tRPL4\nRPL34\tRPL9\nRPL34\tRPS23\nRPL34\tRPL6\nRPL34\tRPL35A\nRPL34\tRPS24\nHNRPDL\tMATR3\nHNRPDL\tSON\nHNRPDL\tNAP1L1\nHNRPDL\tNPM1\nHNRPDL\tSF3B1\nHNRPDL\tIL6ST\nHNRPDL\tSERBP1\nHNRPDL\tSFRS11\nMATR3\tWAC\nMATR3\tSERBP1\nMATR3\tNARS\nMATR3\tXPO1\nMATR3\tYTHDC1\nMATR3\tSFRS11\nMATR3\tSON\nMATR3\tMGEA5\nMATR3\tPAPOLA\nMATR3\tSH3BGRL\nMATR3\tSEPT7\nMATR3\tSEPT2\nMATR3\tTAF7\nMATR3\tNPM1\nMATR3\tPCNP\nMATR3\tRNPC2\nMATR3\tTLOC1\nEIF3S3\tMATR3\nEIF3S3\tHSP90AA1\nEIF3S3\tTTC3\nEIF3S3\tNARS\nEIF3S3\tFNTA\nEIF3S3\tSEPT2\nWAC\tZNF403\nRPL22\tRPL5\nRPL22\tTPT1\nRPL22\tRPS15A\nRPL22\tRPS23\nRPL22\tRPS27A\nRPL22\tRPS7\nRPL22\tRPL30\nRPL22\tRPS3A\nRPL22\tRPL6\nRPL22\tRPL34\nRPL22\tRPL26\nRPL22\tRPL9\nRPL22\tRPL24\nRPL22\tRPS25\nRPL22\tRPL35A\nRPL22\tRPS17\nRPL22\tRPS24\nRPL22\tRPL4\nRPL22\tRPL41\nRPL22\tRPL7\nRPL22\tRPS6\nRPL22\tRPL36A\nRPL22\tRPL23\nRPL22\tRPS4X\nRPL22\tRPL31\nSH3BGRL\tYTHDC1\nSH3BGRL\tTAF7\nSH3BGRL\tSON\nMRPL34\tNDUFS3\nMRPL34\tUQCRFS1\nMRPL34\tUQCRC1\nMRPL34\tNDUFV1\nMRPL34\tPOLR2I\nNDUFS3\tPOLR2I\nNDUFS3\tNDUFS8\nNDUFS3\tUQCRFS1\nFIBP\tSIAHBP1\nFIBP\tWIPI2\nFIBP\tPOLR2E\nAURKAIP1\tCOX5A\nAURKAIP1\tC12orf10\nAURKAIP1\tMRPL34\nAURKAIP1\tGPX4\nAURKAIP1\tNDUFS3\nAURKAIP1\tTUFM\nAURKAIP1\tNDUFS8\nAURKAIP1\tNDUFS7\nAURKAIP1\tNDUFA2\nAURKAIP1\tC3orf60\nAURKAIP1\tNDUFA3\nAURKAIP1\tUQCRFS1\nAURKAIP1\tTMEM93\nAURKAIP1\tUQCRC1\nAURKAIP1\tFIBP\nAURKAIP1\tEXOSC4\nDIP2A\tPGF\nCD52\tIL7R\nCD52\tTRBV3-1\nCD52\tFAIM3\nCD52\tLCK\nCD52\tISG20\nCD52\tTRA@\nCD52\tTRB@\nCD52\tRAC2\nCD52\tTRBC1\nCSNK1A1\tPAPOLA\nCSNK1A1\tEIF4G2\nPAPOLA\tTAF7\nPAPOLA\tPUM2\nPAPOLA\tSON\nPAPOLA\tZNF9\nPAPOLA\tSSR1\nPAPOLA\tSRP72\nPAPOLA\tTRAM1\nPAPOLA\tTHRAP1\nPAPOLA\tTMEM123\nPAPOLA\tUSP34\nPAPOLA\tRNPC2\nPAPOLA\tPNRC2\nPAPOLA\tYTHDC1\nPAPOLA\tSF3B1\nHLA-G\tMAN2B1\nHLA-G\tTGFB1\nHLA-G\tISG20\nHLA-G\tIFI30\nHLA-G\tHLA-J\nMAN2B1\tTGFB1\nATP5D\tCOX5B\nATP5D\tCYC1\nCOX5B\tFLJ14346\nCOX5B\tNDUFS3\nCOX5B\tNDUFB2\nCOX5B\tNDUFA3\nCOX5B\tMRPS12\nCOX5B\tNDUFS8\nCOX5B\tUQCRC1\nCOX5B\tCYC1\nCOX5B\tMSRB2\nCOX5B\tUQCRQ\nCOX5B\tNDUFB11\nCOX5B\tMDH2\nCOX5B\tNDUFV1\nCOX5B\tUQCRFS1\nCOX5B\tNDUFS7\nCOX5B\tNDUFAB1\nCOX5B\tCOX8A\nCOX5B\tUQCR\nIGLJ3\tIGLV3-25\nIGLJ3\tLOC91316\n215176_x_at\tIGHG3\n215176_x_at\t217258_x_at\n215176_x_at\tIGHV1-69\n215176_x_at\t217281_x_at\n215176_x_at\tIGLC2\n215176_x_at\tIGHA1\n215176_x_at\tIGKC\n215176_x_at\tIGHM\n215176_x_at\tIGLV3-25\n215176_x_at\tIGKV1OR2-108\n215176_x_at\tIGLJ3\n215176_x_at\tIGKV1-5\n215176_x_at\tIGKV1OR15-118\n215176_x_at\tIGLC1\n215176_x_at\tIGL@\n215176_x_at\tLOC91316\n215176_x_at\tIGKV1D-13\n215176_x_at\t221651_x_at\n215176_x_at\tCTA-246H3.1\nIGHG3\tIGHM\nIGHG3\tIGHV1-69\nIGHG3\tIGKV1-5\nIGHG3\tIGLJ3\nIGHG3\tIGKV1D-13\nIGHG3\tIGKC\nIGHG3\tIGKV1OR2-108\nIGHG3\tIGLC2\nIGHG3\tIGLC1\nIGHG3\tLOC91316\n217258_x_at\tIGLC2\n217258_x_at\tIGKC\n217258_x_at\tIGL@\n217258_x_at\tCTA-246H3.1\n217258_x_at\tIGLJ3\n217258_x_at\tIGKV1-5\n217258_x_at\tIGLV3-25\nRPL14\tRPL17\nRPL14\tRPS4X\nRPL14\tRPS3A\nRPL14\tRPL5\nRPL14\tRPS7\nRPL14\tU16\nRPL14\tRPL21\nRPL14\tRPL4\nRPL14\tRPL6\nRPL14\tRPL35A\nRPL14\tRPL39\nRPL14\tRPL23\nDYNLT3\tPAFAH1B1\nPAFAH1B1\tPRKACB\nPAFAH1B1\tPOPDC3\nPAFAH1B1\tREEP5\nPAFAH1B1\tZFR\nPAFAH1B1\tPCNP\nPAFAH1B1\tSEPT7\nRPS18\tRPS19\nSF3B1\tWAC\nSF3B1\tSON\nSF3B1\tTUG1\nSF3B1\tSFRS5\nSF3B1\tYTHDC1\nSF3B1\tTCEA1\nIQGAP1\tMBNL1\nIQGAP1\tROCK1\nMBNL1\tOSBPL8\nMBNL1\tROCK1\nMBNL1\tPNRC2\nMBNL1\tPUM2\nNPM1\tRPS3A\nNPM1\tSFRS5\nNPM1\tSFRS11\nNPM1\tRPS7\nNPM1\tRPS6\nNPM1\tRPL7\nNPM1\tRPL34\nNPM1\tTCEA1\nNPM1\tTUG1\nNPM1\tRPL5\nNPM1\tRPL17\nNPM1\tRPL24\nNPM1\tSF3B1\nNPM1\tTINP1\nNPM1\tRPL6\nNPM1\tRPS24\nNPM1\tRPL4\nNPM1\tRPS27A\nNPM1\tRNPC2\nRPS3A\tSF3B1\nRPS3A\tTPT1\nRPS3A\tU16\nRPS3A\tRPS4X\nRPS3A\tRPS7\nRPS3A\tRPS6\nSPN\tZNF552\nDDX50\tRNPC2\nEIF5\tPCNP\nEIF5\tPAPOLA\nEIF5\tTAF7\nEIF5\tMATR3\nEIF5\tHSP90AA1\nPCNP\tSEPT2\nPCNP\tTLOC1\nPCNP\tRNPC2\n217281_x_at\tIGHM\n217281_x_at\tIGHV1-69\n217281_x_at\tIGLJ3\n217281_x_at\tIGKC\n217281_x_at\tIGLC2\n217281_x_at\tIGHA1\n217281_x_at\tIGHG3\n217281_x_at\tIGKV1OR2-108\nCHCHD2\tMIF\nCHCHD2\tNDUFAB1\nCHCHD2\tCOX5B\nCHCHD2\tMRPS12\nCHCHD2\tMRP63\nCHCHD2\tCOX8A\nCHCHD2\tSLC25A3\nCHCHD2\tNDUFB11\nMIF\tTIMM13\nARPC5\tCAP1\nARPC5\tPRG1\nSFRS11\tTUG1\nACTR10\tSKP1A\nACTR10\tPRKAR1A\nSKP1A\tSUMO2\nACTR2\tARPC3\nACTR2\tACTR3\nACTR2\tCAPZA1\nACTR2\tPDCD10\nARPC3\tCAPZA1\nCAV1\tMMP2\nCAV1\tCOL6A1\nCAV1\tFBLN1\nCAV1\tCOL1A2\nCAV1\tPTRF\nCAV1\tLAMC1\nACTG2\tMYH11\nMYH11\tMYLK\nMYH11\tTPM1\nMYH11\tTAGLN\nRPL38\tRPS20\nRPL38\tRPLP2\nRPL38\tRPS19\nRPS20\tRPS27\nCSDE1\tFLJ11021\nCSDE1\tWAC\nCSDE1\tNFE2L2\nCOL1A2\tCOL3A1\nCOL3A1\tCOL6A3\nCD7\tLCK\nCD7\tHMHA1\nLCK\tRAC2\nLCK\tTRA@\nLCK\tTRB@\nLCK\tTRBC1\nLCK\tTRBV3-1\nNDUFB11\tNDUFS3\nNDUFB11\tUQCRQ\nNDUFB11\tUQCR\nNDUFB11\tNDUFB4\nNDUFB11\tUQCRFS1\nNDUFB11\tSLC25A3\nNDUFB11\tNDUFS8\nNDUFB11\tPOLR2L\nC3orf60\tEXOSC4\nC3orf60\tPOLR2I\nC3orf60\tMRPL34\nC3orf60\tGPX4\n211639_x_at\tIGKC\nRPL11\tRPS15A\nRPL11\tRPL5\nRPL11\tRPS24\nRPL11\tRPL17\nRPL11\tRPL7\nRPL11\tU16\nRPL11\tRPL24\nRPL11\tRPL21\nRPL11\tRPL35A\nRPL11\tRPS25\nRPL11\tRPL14\nRPL11\tRPS6\nRPL11\tRPL22\nRPL11\tRPL34\nRPL11\tRPL4\nRPL11\tRPS4X\nRPL11\tRPS3A\nRPL11\tRPS7\nRPL11\tRPL39\nRPL11\tRPL31\nRPL11\tRPL36A\nRPL11\tRPL23\nRPL11\tRPS17\nRPL11\tRPL30\nRPL11\tRPS23\nRPS15A\tRPS6\nRPS15A\tRPS17\nRPS15A\tRPS7\nRPS15A\tRPS3A\nRPS15A\tRPS20\nRPS15A\tRPS27\nRPS15A\tRPS23\nGNB2L1\tRPL13A\nGNB2L1\tRPS19\nGNB2L1\tRPL29\nGNB2L1\tRPS16\nRPL29\tRPS9\nRPL29\tRPS16\nRPL29\tRPL35\nRPL13\tRPL18A\nRPL13\tRPL28\nRPL13\tRPS2\nRPL13\tRPL18\nRPL13\tRPLP2\nRPL13\tRPS15\nRPL18A\tRPS2\nRPL18A\tRPS19\nRPL18A\tRPLP2\nRPL18A\tRPL28\nHLA-A\tHLA-F\nHLA-A\tMAN2B1\nHLA-A\tHLA-E\nHLA-A\tHLA-G\nHLA-A\tHLA-B\nHLA-A\tHLA-J\nHLA-A\tHLA-C\nHLA-F\tISG20\nHLA-F\tHLA-G\nHLA-F\tIFI30\nHLA-F\tMAN2B1\nHLA-F\tHLA-J\nBTF3\tMDS1\nBTF3\tRPS27\nBTF3\tRPS17\nBTF3\tRPS15A\nBTF3\tRPL30\nBTF3\tLOC402057\nBTF3\tRPL22\nNACA\tRPL35A\nNACA\tRPS3A\nNACA\tRPL5\nNACA\tNAP1L1\nNACA\tSFRS5\nNACA\tRPS7\nNACA\tRPL24\nNACA\tU16\nNACA\tRPL21\nNACA\tRPL9\nNACA\tNPM1\nNACA\tRPS23\nNACA\tRPL4\nNACA\tRPS27A\nNACA\tRPS24\nNACA\tRPL17\nNACA\tRPL6\nNACA\tTINP1\nRPL35A\tRPL7\nRPL35A\tRPS17\nRPL35A\tRPS27A\nRPL35A\tRPS20\nRPL35A\tRPS27\nRPL35A\tRPS15A\nRPL35A\tRPS3A\nRPL35A\tRPS25\nRPL35A\tRPL36A\nRPL35A\tRPL39\nRPL35A\tRPL6\nRPL35A\tRPS6\nRPL35A\tRPS4X\nRPL35A\tRPL4\nRPL35A\tRPL5\nRPL35A\tRPS23\nRPL35A\tRPS24\nRPL35A\tU16\nRPL35A\tRPL9\nRPL35A\tRPS7\nBIRC2\tDDX5\nBIRC2\tZNF403\nBIRC2\tLOC58486\nBIRC2\tTHRAP1\nBIRC2\tSERBP1\nBIRC2\tPAPOLA\nBIRC2\tUSP34\nBIRC2\tC1orf63\nDDX5\tSERBP1\nDDX5\tSRP46\nDDX5\thfl-B5\nDDX5\tTUG1\nDDX5\tNAP1L1\nDDX5\tHNRPH1\nDDX5\tNBPF1\nDDX5\tSFRS5\nDDX5\tLOC342346\nDDX5\tNBPF12\nDDX5\tMGEA5\nDDX5\tHNRPA2B1\nDDX5\tSON\nDDX5\tFLJ10154\nDDX5\tUSP34\nDDX5\tNPM1\nDDX5\tTARDBP\nDDX5\tEIF4G2\nDDX5\tPAPOLA\nDDX5\tHIATL1\nDDX5\tYTHDC1\nDDX5\tSF3B1\nRPL30\tU16\nRPL30\tRPS7\nRPL30\tRPS25\nRPL30\tRPS13\nRPL30\tRPS3A\nRPL30\tRPS4X\nRPL30\tRPL4\nRPL30\tRPS6\nRPL30\tRPL37\nRPL30\tRPL31\nRPL30\tRPL36A\nRPL30\tRPL35A\nRPL30\tRPS27\nRPL30\tRPS17\nRPL30\tRPS15A\nRPL30\tRPL9\nRPL30\tRPS20\nRPL30\tRPS23\nRPL30\tRPL32\nRPL30\tRPL39\nTMEM123\tTRAM1\nIGHA1\tIGHV1-69\nIGHA1\tIGKC\nIGHA1\tIGLJ3\nIGHA1\tIGLV3-25\nIGHA1\tIGHM\nIGHA1\tIGKV1OR15-118\nIGHA1\tIGLC2\nIGHA1\tIGL@\nIGHA1\tIGKV1-5\nIGHA1\tIGKV1D-13\nIGHA1\tIGHG3\nIGHA1\tIGKV1OR2-108\nIGHA1\tLOC91316\nIGHA1\tIGLC1\nIGHV1-69\tIGLC2\nIGHV1-69\tIGKC\nIGHV1-69\tIGLC1\nIGHV1-69\tIGKV1OR2-108\nNDUFS7\tPOLR2I\nNDUFS7\tNDUFV1\nNDUFS7\tNDUFS8\nPOLR2I\tUQCRFS1\n214836_x_at\t217258_x_at\n214836_x_at\t217281_x_at\n214836_x_at\tIGLC2\n214836_x_at\tIGLV3-25\n214836_x_at\tIGL@\n214836_x_at\tIGKV1OR2-108\n214836_x_at\tIGKV1OR15-118\n214836_x_at\tIGLJ3\n214836_x_at\tIGHG3\n214836_x_at\tIGKC\n214836_x_at\tLOC91316\n214836_x_at\tIGKV1-5\n214836_x_at\tIGKV1D-13\n214836_x_at\tIGLC1\n214836_x_at\tIGHA1\n214836_x_at\t221651_x_at\n214836_x_at\tCTA-246H3.1\n214836_x_at\t215176_x_at\nRPS6\tTPT1\nRPS6\tRPS7\nRPS6\tU16\nGTF2I\tSRP9\nGTF2I\tTMEM66\nSRP9\tSUMO1\nATP2A3\tTRA@\nATP2A3\tTRB@\nATP2A3\tTRBC1\nTRA@\tTRBC1\nTRA@\tTRB@\nIGLC2\tIGLJ3\nIGLC2\tLOC91316\nIGLC2\tIGLV3-25\nIGLC2\tIGLV3-10\nHSP90B1\tZNF638\nHSP90B1\tRNPC2\nHSP90B1\tVDP\nRPL7\tTPT1\nRPL7\tRPS3A\nRPL7\tRPS6\nRPL7\tRPS24\nRPL7\tTCEA1\nRPL7\tSF3B1\nRPL7\tRPS7\nRPL7\tRPS25\nRPL7\tRPL9\nRPL7\tRPS23\nRPL7\tRPS27A\nRPL7\tRPS4X\nMGEA5\tNAP1L1\nMGEA5\tPAPOLA\nMGEA5\tXPO1\nMGEA5\tSF3B1\nMGEA5\tRNPC2\nMGEA5\tYTHDC1\nMGEA5\tPNRC2\nBCLAF1\tPCM1\nBCLAF1\tRNPC2\nBCLAF1\tPUM2\nBCLAF1\tEIF3S6\nBCLAF1\tUSP47\nBCLAF1\tSF3B1\nBCLAF1\tSLC38A2\nBCLAF1\tFLJ11021\nBCLAF1\tCOPS2\nBCLAF1\tPCNP\nBCLAF1\tCSDE1\nBCLAF1\tMATR3\nBCLAF1\tWAC\nBCLAF1\tCROP\nBCLAF1\tSRP72\nBCLAF1\tLOC58486\nBCLAF1\tPHIP\nBCLAF1\tZNF638\nBCLAF1\tVDP\nBCLAF1\tNPM1\nBCLAF1\tHSP90B1\nBCLAF1\tTLOC1\nPCM1\tVDP\nCXorf9\tGPSM3\nCXorf9\tTRA@\nCXorf9\tIL2RG\nCXorf9\tIL16\n216342_x_at\tRPL7\n216342_x_at\tTPT1\n216342_x_at\tRPL14\n216342_x_at\tRPL9\n216342_x_at\tRPS27A\n216342_x_at\tRPL17\n216342_x_at\tRPS17\n216342_x_at\tRPL5\n216342_x_at\tU16\n216342_x_at\tRPL22\n216342_x_at\tRPL36A\n216342_x_at\tRPS25\n216342_x_at\tLOC402057\n216342_x_at\tRPS7\n216342_x_at\tRPL21\n216342_x_at\tRPL24\n216342_x_at\tRPL41\n216342_x_at\tRPS24\n216342_x_at\tMDS1\n216342_x_at\tRPL6\n216342_x_at\tLOC440055\n216342_x_at\tRPL4\n216342_x_at\tRPL34\n216342_x_at\tRPS3A\n216342_x_at\tRPL11\n216342_x_at\tRPS6\n216342_x_at\tRPL23\n216342_x_at\tRPL30\n216342_x_at\tRPL31\n216342_x_at\tRPS23\n216342_x_at\tRPL35A\n216342_x_at\tRPS4X\nNDUFA13\tNDUFA3\nNDUFA13\tNDUFB2\nNDUFA3\tNDUFAB1\nNDUFA3\tUCRC\nNDUFA3\tNDUFB2\nNDUFA3\tNDUFB11\nNDUFA3\tUQCR\nNDUFA3\tUQCRC1\nNDUFA3\tNDUFB4\nCDKN1B\tPPP2CA\nCDKN1B\tMARCKS\nHMGB1\tHNRPR\nHMGB1\tHNRPA2B1\nHNRPR\tSRP9\nHNRPR\tSON\nHNRPR\tSFRS10\nAP2S1\tATP6V0C\nAP2S1\tPOLR2E\nAP2S1\tGPX1\nAP2S1\tGUK1\nAP2S1\tNAPA\nAP2S1\tCOPE\nAP2S1\tATP6V0B\nAP2S1\tMRPS12\n211637_x_at\tIGKC\n211637_x_at\tIGHA1\n211637_x_at\tIGLC2\n211637_x_at\t211639_x_at\n211637_x_at\tIGHM\n211637_x_at\t211908_x_at\n211637_x_at\tIGHV1-69\n211637_x_at\tCTA-246H3.1\n211637_x_at\t211641_x_at\n211637_x_at\t215176_x_at\n211637_x_at\t211645_x_at\n211637_x_at\tIGHG3\n211637_x_at\t217281_x_at\n211637_x_at\tLOC91316\nCOL4A1\tCOL4A2\nCOL4A1\tFBLN1\nCOL4A2\tRBPMS\nFLJ42393\tPDE4C\nFLJ42393\tZNF160\nFLJ42393\tPOLR1B\nFLJ42393\tKNS2\nFLJ42393\tSLC35E1\nFLJ42393\tLOC152719\nMORF4L1\tPTGES3\nMORF4L1\tSKP1A\nPTGES3\tSUMO2\nSON\tXPO1\nSON\tSRP46\nSON\tYTHDC1\nIFI30\tTGFB1\nIFI30\tNCF1\nAASDHPPT\tPOPDC3\nAASDHPPT\tPAFAH1B1\nPOPDC3\tRAB6C\nPOPDC3\tPRKACB\nFXYD5\tITGB2\nFXYD5\tPSMB9\nCD79A\tIGKC\nCD79A\tIGHA1\nCD79A\tIGLC2\nCD79A\tIGLJ3\nFLJ14346\tZNF611\nFLJ14346\tGPAA1\nFLJ14346\tMRPS12\nFLJ14346\tLOC152719\nFLJ14346\tSLC35E1\nHLA-DPA1\tHLA-DQB1\nHLA-DPA1\tHLA-DQA1\nHLA-DPA1\tHLA-DRA\nHLA-DPA1\tHLA-DRB5\nHLA-DPA1\tHLA-DRB4\nHLA-DPA1\tHLA-DRB1\nHLA-DPA1\tHLA-DPB1\nHLA-DQB1\tHLA-DRB4\nHLA-DQB1\tHLA-DRB5\nHLA-DQB1\tHLA-DRB1\nIFNGR1\tROCK1\nC6orf12\tHLA-F\nC6orf12\tHLA-G\nC6orf12\tHLA-B\nC6orf12\tHLA-C\nC6orf12\tHLA-J\nC6orf12\tHLA-A\nNARS\tTLOC1\nNARS\tTTC3\nNARS\tSRP9\nRPL36A\tRPL6\nRPL36A\tRPS7\nRPL36A\tRPS24\nRPL36A\tRPS3A\nRPL36A\tRPL4\nRPL36A\tRPS4X\nRPL36A\tRPS25\nRPL36A\tRPS15A\nRPL36A\tRPS6\nRPL36A\tRPS17\nRPL36A\tRPS23\nRPL36A\tRPL39\n217718_s_at\tYWHAZ\n217718_s_at\tCAB39\nRPL23A\tRPS20\nTAF7\tUBE4A\nTAF7\tYTHDC1\nTAF7\tXPO1\nDHX15\tOGT\nDHX15\tKIAA0907\nDHX15\tNPM1\nCD2\tTRA@\nCD2\tCD52\nCD2\tIL7R\nDST\tPLEKHC1\nDST\tSPARCL1\nCOL6A1\tCOL6A3\nCOL6A1\tFBLN1\nCOL6A1\tPTRF\nFIS1\tNDUFA13\nEIF4G2\tSRP46\nEIF4G2\tHSP90AA1\nEIF4G2\tSERBP1\nEIF4G2\tSON\nEIF4G2\tEIF5\nEIF4G2\tNPM1\nEIF4G2\tPUM2\nEIF4G2\tSET\nEIF4G2\tPCNP\nEIF4G2\tLOC342346\nEIF4G2\tHNRPA2B1\nEIF4G2\tYTHDC1\nEIF4G2\tXPO1\nEIF4G2\tMATR3\nEIF4G2\tPAPOLA\nEIF4G2\tTHRAP1\nEIF4G2\tSF3B1\nEIF4G2\tHIATL1\nSRP46\tXPO1\nCOL1A1\tCOL6A3\nCOL1A1\tCOL1A2\nCOL1A1\tCOL3A1\nC12orf10\tFIBP\nC12orf10\tC3orf60\nLMOD1\tMYH11\n208246_x_at\tPOLR1B\n208246_x_at\tZNF611\n208246_x_at\tPGF\n208246_x_at\tATP8B1\n208246_x_at\tRIOK3\n208246_x_at\tKNS2\n208246_x_at\tDBT\n208246_x_at\tLOC152719\n208246_x_at\tFBXW12\n208246_x_at\tPDE4C\n208246_x_at\tSLC35E1\n208246_x_at\tZNF160\n208246_x_at\t216524_x_at\nHNRPH1\tSON\nHNRPH1\tSF3B1\nHNRPH1\tYTHDC1\nRPS14\tRPS16\nRPS14\tRPS5\nRPS14\tRPS9\nRPS16\tRPS19\nRPS16\tRPS9\nRPS16\tRPS5\nARHGDIB\tLAPTM5\nARHGDIB\tPRG1\nARHGDIB\tUCP2\nARHGDIB\tRAC2\nARHGDIB\tCXCR4\nARHGDIB\tCECR1\nLAPTM5\tRAC2\nLAPTM5\tPRG1\nATRX\tCOPS2\nATRX\tPCNP\nATRX\tPUM2\nATRX\tMATR3\nATRX\tSH3BGRL\nATRX\tSEPT7\nATRX\tBCLAF1\nATRX\tTLOC1\nHIATL1\tSFRS5\nHIATL1\tRPL6\nHIATL1\tRPL17\nHIATL1\tTUG1\nHIATL1\tHNRPA2B1\nHIATL1\tRAVER2\nHIATL1\tRNPC2\nHIATL1\tHNRPH1\nHIATL1\tYTHDC1\nHIATL1\tTCEA1\nHIATL1\tNPM1\nHIATL1\tLOC58486\nHIATL1\tTHRAP1\nHIATL1\tPAPOLA\nHIATL1\tSF3B1\nFAIM3\tTRA@\nFAIM3\tHCLS1\nFAIM3\tRAC2\nIGKV1D-13\tIGLJ3\nIGKV1D-13\tIGLV3-25\nIGKV1D-13\tIGLC2\nIGKV1D-13\tLOC91316\nIGKV1D-13\tIGL@\nIGKV1D-13\tIGLC1\nIGKV1D-13\tIGKV1OR15-118\nHSP90AA1\tMATR3\nHSP90AA1\tSRP9\nHSP90AA1\tSERBP1\nHSP90AA1\tSEPT2\nHSP90AA1\tNARS\nRPL19\tRPS20\nLOC283412\tRPL29\nLOC283412\tRPS9\nDGCR6\tPOLR2I\nLTB\tTRBC1\nLTB\tTRB@\nHMHA1\tPSMB8\nHMHA1\tTRBC1\nHMHA1\tRAC2\nPSMB8\tTAPBP\nPSMB8\tPSMB9\nIGKV1-5\tIGL@\nIGKV1-5\tIGKV1OR2-108\nIGKV1-5\tIGLC2\nIGKV1-5\tIGLJ3\nIGKV1-5\tIGLV3-25\nIGKV1-5\tIGLC1\nIGKV1-5\tIGKV1D-13\nIGKV1-5\tIGKV1OR15-118\nRPL28\tRPS10\nRPL28\tRPS2\nRPL28\tRPS19\nRPL28\tRPS16\nRPL28\tRPLP2\nHCLS1\tITGB2\nHCLS1\tSELL\nHCLS1\tRAC2\nHCLS1\tTRA@\nFBLN1\tMMP2\nFBLN1\tPTRF\nNDUFA2\tUQCRFS1\nNDUFA2\tUQCR\nNDUFA2\tNDUFS8\nNDUFA2\tUQCRC1\nNDUFA2\tNDUFB4\nNDUFA2\tTCEB2\nNDUFA2\tNDUFA3\nNDUFA2\tUCRC\nHLA-DRB4\tHLA-DRB5\nFAM96B\tNDUFV1\nFAM96B\tNDUFS8\nFAM96B\tNDUFS3\nFAM96B\tTUFM\nFAM96B\tNDUFS7\nFAM96B\tNDUFA10\nFAM96B\tNDUFA2\nFAM96B\tNDUFA3\nCOQ9\tNDUFB4\nCOQ9\tUQCRFS1\nEEF1B2\tRPL30\nEEF1B2\tRPL10A\nEEF1B2\tRPS23\nEEF1B2\tRPS15A\nEEF1B2\tRPL35A\nEEF1B2\tRPL14\nEEF1B2\tRPL36A\nEEF1B2\tRPL37\nEEF1B2\tRPL39\nEEF1B2\tLOC440055\nPUM2\tWAC\nPUM2\tSF3B1\nALOX5AP\tLAPTM5\nALOX5AP\tITGB2\nDKFZP566N034\tZNF160\nDKFZP566N034\tSLC35E1\nARL6IP5\tPAFAH1B1\nTRB@\tTRBV3-1\nTRB@\tTRBC1\nIFI16\tROCK1\nIFI16\tIQGAP1\nIFI16\tTRIM22\nIFI16\tMBNL1\nIFI16\tIFNGR1\n211641_x_at\t211908_x_at\n211641_x_at\t211645_x_at\n211641_x_at\tIGHG3\n211641_x_at\tIGKV1-5\n211641_x_at\t217281_x_at\n211641_x_at\tIGHA1\n211641_x_at\tIGHM\n211908_x_at\tIGHV1-69\n211908_x_at\t217281_x_at\n211908_x_at\tIGHM\nWSB1\tXPO1\nARPC1B\tHLA-C\nARPC1B\tHLA-A\nARPC1B\tHLA-B\nARPC1B\tISG20\nARPC1B\tHLA-G\nCD53\tITGB2\nCD53\tIL10RA\nCD53\tHCLS1\nHLA-E\tHLA-F\nHLA-E\tPSMB8\nHLA-E\tHLA-J\nHLA-E\tMAN2B1\nHLA-E\tHLA-G\n211645_x_at\tIGHA1\n211645_x_at\tIGKC\n211645_x_at\tIGHM\n211645_x_at\tIGLC2\n211645_x_at\tIGHG3\n211645_x_at\tIGL@\n211645_x_at\tIGLV3-25\n211645_x_at\tIGLJ3\n211645_x_at\tLOC91316\n211645_x_at\tIGLC1\n211645_x_at\tIGKV1-5\n211645_x_at\t221651_x_at\n211645_x_at\tIGKV1OR15-118\n211645_x_at\tCTA-246H3.1\n211645_x_at\tIGKV1D-13\n211645_x_at\t214836_x_at\n211645_x_at\t215176_x_at\nNDUFB2\tNDUFS7\nNDUFB2\tUQCRC1\nNDUFB2\tUQCR\nNDUFB2\tNDUFV1\nFLJ10154\tLOC58486\nFLJ10154\tXPO1\nFLJ10154\tPFAAP5\nFLJ10154\tRNPC2\nFLJ10154\tHNRPDL\nFLJ10154\tSFPQ\nFLJ10154\tSFRS11\nFLJ10154\tNPM1\nEIF4A2\tTTC3\nEIF4A2\tMATR3\nGPX4\tR3HCC1\nGPX4\tPOLR2I\nGPX4\tMRPL34\nPRKAR2A\tSLC35E1\nSLC35E1\tUBE2D2\nSLC35E1\tWDR45\nSLC35E1\tZNF611\nSLC35E1\tZNF160\nIGLC1\tIGLJ3\nIGLC1\tIGLV3-25\nIGLC1\tIGLC2\nIGLC1\tLOC91316\nUQCRC1\tUQCRFS1\nTMED5\tWAC\nMDH2\tNDUFB11\nMDH2\tUQCRQ\nMDH2\tUQCRC1\nMDH2\tUQCRFS1\nMDH2\tNDUFS8\nIGL@\tIGLV3-25\nIGL@\tIGLC2\nIGL@\tIGLC1\nIGL@\tIGLJ3\nHLA-DRB1\tHLA-F\nHLA-DRB1\tHLA-DRB4\nHLA-DRB1\tHLA-DRB5\nCD74\tHLA-DQB1\nCD74\tHLA-DRB4\nCD74\tHLA-DRB1\nCD74\tHLA-DRA\nCD74\tHLA-DMA\nCD74\tHLA-DPB1\nCD74\tHLA-DRB5\nCD74\tHLA-DPA1\nCLTC\tNARS\nRPS17\tRPS25\nRPS17\tRPS4X\nRPS17\tRPS6\nRPS17\tRPS7\nRPS17\tRPS3A\nRPS17\tRPS20\nRPS17\tRPS23\nRPS17\tRPS27\nACTR3\tCAP1\nACTR3\tARPC3\nACTR3\tYWHAZ\nACTR3\tCAPZA1\nRPS10\tRPS16\nRPS10\tRPS21\nRPS10\tRPS19\nRPL27A\tRPS11\nRPL27A\tRPLP2\nRPL27A\tRPS19\nRPL27A\tRPL38\nRPL27A\tRPS20\nRPL27A\tRPS18\nDDX17\tSON\nDDX17\tHNRPH1\nC11orf58\tYTHDC1\nC11orf58\tHNRPK\nC11orf58\tLOC342346\nC11orf58\tSON\nNAPA\tWIPI2\nRGPD5\tTTC3\nCD3D\tTRA@\nCD3D\tTRB@\nCD3D\tTRBC1\nIGKV1OR2-108\tLOC91316\nIGKV1OR2-108\tIGLC2\nIGKV1OR2-108\tIGLC1\nIGKV1OR2-108\tIGLJ3\nCAB39\tSUB1\nATP6V0B\tGPX1\nATP6V0B\tCOPE\nATP6V0B\tMRPS12\nATP6V0B\tATP6V0D1\nATP6V0B\tATP6V0C\nGPX1\tPPP1CA\nGPX1\tPGLS\nGPX1\tOAZ1\nENDOG\tNDUFS7\nLOC388344\tRPS2\nLOC388344\tRPL18\nLOC388344\tRPL18A\nLOC388344\tRPL13\nLOC388344\tRPS15\nLOC440055\tRPL22\nLOC440055\tRPL17\nLOC440055\tRPS7\nLOC440055\tRPL9\nLOC440055\tRPS3A\nLOC440055\tRPS15A\nLOC440055\tRPL21\nLOC440055\tRPS4X\nLOC440055\tU16\nLOC440055\tRPL31\nLOC440055\tRPL37\nLOC440055\tRPL35A\nLOC440055\tMDS1\nLOC440055\tRPS6\nLOC440055\tRPS17\nLOC440055\tRPL32\nLOC440055\tRPL23\nLOC440055\tRPL4\nLOC440055\tRPL39\nLOC440055\tRPS20\nLOC440055\tRPL36A\nLOC440055\tRPL30\nLOC440055\tRPS23\nLOC440055\tRPL11\nCYBA\tIFI30\nCYBA\tHLA-G\nCYBA\tHLA-B\nCYBA\tHLA-J\nCYBA\tTGFB1\nRBPMS\tTGFB1I1\nKNS2\tPRR11\nKNS2\tSPG21\nKNS2\tLOC152719\nKNS2\tPGF\nKNS2\tUBE2D2\nKNS2\tPRKAR2A\nKNS2\tRIOK3\nKNS2\tZNF611\nKNS2\tZNF160\nKNS2\tPDE4C\nKNS2\tSLC35E1\nNPEPPS\tSEPT7\nMRPS12\tZNHIT1\nGLTSCR2\tRPS2\nGLTSCR2\tRPL13\nGLTSCR2\tLOC388344\nHLA-DMB\tHLA-DRA\n220725_x_at\tCDC5L\n220725_x_at\tSLC25A16\n220725_x_at\tC12orf38\n220725_x_at\tOPHN1\n220725_x_at\tMCM3AP\n220725_x_at\tLOC56902\nCDC5L\tLOC56902\nCDC5L\tMCM3AP\nARL8B\tHSP90AA1\nARL8B\tNARS\nCTA-246H3.1\tIGLC2\nCTA-246H3.1\tIGLV3-25\nCTA-246H3.1\tIGKC\nCTA-246H3.1\tIGKV1OR15-118\nCTA-246H3.1\tIGHM\nCTA-246H3.1\tIGLJ3\nCTA-246H3.1\tIGKV1OR2-108\nCTA-246H3.1\tIGL@\nCTA-246H3.1\tIGHG3\nCTA-246H3.1\tIGKV1-5\nCTA-246H3.1\tIGKV1D-13\nCTA-246H3.1\tLOC91316\nCTA-246H3.1\tIGLC1\nCTA-246H3.1\tIGHA1\n218041_x_at\tBCLAF1\n218041_x_at\tPUM2\n218041_x_at\tSLC38A2\nMEFV\tSPN\nSMTN\tTGFB1I1\nZNF160\tZNF611\nTUFM\tUQCRFS1\nTUFM\tUQCRC1\nCD164\tCOPB\nARF5\tPOLR2J\nARF5\tATP6V0D1\nHLA-B\tMYH9\nHLA-B\tMAN2B1\nHLA-B\tHLA-G\nHLA-B\tHLA-F\nHLA-B\tHLA-E\nHLA-B\tHLA-C\nHLA-B\tHLA-J\nIGHD\tIGHM\nADRM1\tEXOSC4\nADRM1\tAP2S1\nADRM1\tFIBP\nADRM1\tAURKAIP1\n211650_x_at\tIGLV3-25\n211650_x_at\tIGHG3\n211650_x_at\tIGKC\n211650_x_at\tIGHV1-69\n211650_x_at\tIGHM\nFLJ12151\tPPP2CA\nFLJ12151\tLOC152719\nFLJ12151\tPRR11\nFLJ12151\tPDE4C\nFLJ12151\tZNF611\nFLJ12151\tSLC35E1\nFLJ12151\tZNF160\nHNRPK\tHNRPU\nHNRPK\tSET\nTOMM20\tZMYND11\nTOMM20\tTTC3\nTOMM20\tTTC19\nCORO1A\tLAPTM5\nAD7C-NTP\tARL6IP2\nAD7C-NTP\tMEFV\nAD7C-NTP\tCDC5L\nARL6IP2\tCDC5L\nARL6IP2\tLOC56902\nARL6IP2\tMEFV\nC1orf63\tPAPOLA\nC1orf63\tLOC342346\nRPL31\tRPL5\nRPL31\tTPT1\nRPL31\tRPS27A\nRPL31\tRPS7\nRPL31\tRPS17\nRPL31\tRPL9\nRPL31\tRPS3A\nRPL31\tRPL6\nRPL31\tRPL35A\nRPL31\tRPL36A\nRPL31\tRPL7\nRPL31\tRPS6\nRPL31\tRPL4\nRPL31\tRPS24\nRPL31\tRPS23\nRPL31\tRPL34\nRPL31\tRPS4X\nSDHA\tTUFM\nUQCR\tUQCRQ\nUQCR\tUQCRC1\nFNBP4\tRPL26\nFNBP4\tRNPC2\nFNBP4\tRPL17\nFNBP4\tOGT\nFNBP4\tRPL6\nFNBP4\tRPS27A\nPNRC2\tZNF638\nPNRC2\tPUM2\nPNRC2\tSF3B1\nPNRC2\tROCK1\nIGLV3-10\tIGLV3-25\nMSN\tPRG1\nPPIB\tRPN1\nPPIB\tSEC61A1\nTHRAP1\tZNF638\nIRF7\tPRKD2\nIRF7\tMAN2B1\n221651_x_at\tIGLJ3\n221651_x_at\tIGKV1OR15-118\n221651_x_at\tIGLC2\n221651_x_at\tIGKV1-5\n221651_x_at\tIGKC\n221651_x_at\tIGKV1OR2-108\n221651_x_at\tIGHG3\n221651_x_at\tLOC91316\n221651_x_at\tIGKV1D-13\n221651_x_at\tCTA-246H3.1\n221651_x_at\tIGHA1\n221651_x_at\tIGLC1\nLOC152719\tPRR11\nLOC152719\tLRRFIP1\nLOC152719\tPDE4C\nLOC152719\tRIOK3\nLOC152719\tZNF611\nLOC152719\tSLC35E1\nLOC152719\tZNF160\nPRKAR1A\tRNF11\nPRKAR1A\tSRP9\nPRKAR1A\tTMEM66\nINPP5D\tITGB2\n216524_x_at\tZNF611\n216524_x_at\tZNF160\n216524_x_at\tPGF\n216524_x_at\tKNS2\n216524_x_at\tCEP27\n216524_x_at\tRIOK3\n216524_x_at\tDBT\n216524_x_at\tPDE4C\n216524_x_at\tFBXW12\nRPL10A\tRPL39\nRPL10A\tRPL14\nRPL10A\tRPL12\nRPL27\tRPS20\nRPL27\tRPS11\nRPL27\tRPLP2\nRPL27\tRPL38\nRPL27\tRPS19\nRPL27\tRPL27A\nERH\tHNRPK\nERH\tSUMO2\nPGF\tPOLR1B\nPGF\tPRR11\nPGF\tZNF160\nPGF\tWDR45\nPGF\tRIOK3\nPGF\tZNF611\nCAPNS1\tCLPTM1\nCLPTM1\tNAPA\nRPS19\tRPS2\nRPS19\tRPS21\nRPS19\tRPS20\nDDX42\tMT1H\nDDX42\tMT1M\nDDX42\tMT1G\nDDX42\tMT1F\nMT1H\tMT1L\nMT1H\tMT2A\nMT1H\tMT1M\nMT1H\tMT1X\nCD37\tPTPRCAP\nCD37\tISG20\nPTPRCAP\tTRBV3-1\nEIF3S6\tRPS4X\nEIF3S6\tRPL26\nEIF3S6\tZNF638\nEIF3S6\tMDS1\nEIF3S6\tRPS3A\nEIF3S6\tRPL24\nEIF3S6\tRPL7\nEIF3S6\tRPL31\nEIF3S6\tRPL4\nEIF3S6\tRPL21\nEIF3S6\tU16\nEIF3S6\tNPM1\nEIF3S6\tRNPC2\nEIF3S6\tNACA\nEIF3S6\tRPS7\nEIF3S6\tTINP1\nEIF3S6\tVDP\nEIF3S6\tRPS27A\nEIF3S6\tRPL34\nEIF3S6\tRPL5\nEIF3S6\tRPS24\nEIF3S6\tRPL17\nEIF3S6\tRPL6\nIGKV1OR15-118\tIGL@\nIGKV1OR15-118\tIGLC2\nIGKV1OR15-118\tIGLV3-25\nNDUFB6\tTCEB2\nACTA2\tLMOD1\nACTA2\tMYH11\nACTA2\tTAGLN\nGMFG\tITGB2\nHNRPU\tSET\nHNRPH3\tLOC58486\nHNRPH3\tRNPC2\nHNRPH3\tMATR3\nHNRPH3\tKIDINS220\nC16orf24\tSCAND1\nNBPF1\tNBPF12\nARPP-19\tPRKACB\n217052_x_at\tG3BP\n217052_x_at\t220725_x_at\nCCL5\tHLA-F\nCCL5\tHLA-C\nCCL5\tHLA-G\nRPLP2\tRPS16\nRPLP2\tRPS20\nRPLP2\tRPS19\nRPLP2\tRPS11\nECH1\tUQCRQ\nRPL12\tRPL14\nRPL12\tRPL39\nRPL12\tRPL19\nCECR1\tLAPTM5\nATP6V0D1\tCHMP2A\nATP6V0D1\tPOLR2J\nATP6V0D1\tPSMB3\nATP6V0D1\tOAZ2\nCBX3\tHNRPA2B1\nCBX3\tXPO1\nSFRS10\tSON\nTIMM8B\tUQCRC1\nCD48\tIL16\nARF1\tCOPE\nARF1\tYIPF3\nARF1\tCAPNS1\nARF1\tRPN1\nKPNB1\tPAFAH1B1\nHLA-J\tIFI30\nHLA-J\tMAN2B1\nUSP47\tZNF638\nUSP47\tVDP\nRPL36\tRPS16\nRPL36\tRPS18\nRPL36\tRPS19\nRPL36\tRPS10\nCUL4B\tSH3BGRL\nADD3\tWAC\nADD3\tSEPT7\nADD3\tMATR3\nDEXI\tNDUFB11\nLYSMD4\tSEPT7\nPFAAP5\tZNF638\nPFAAP5\tRNPC2\n215182_x_at\tSPN\n215182_x_at\tCDC5L\n215182_x_at\t220725_x_at\n215182_x_at\tLOC56902\n215182_x_at\tMCM3AP\nHLA-DQA1\tHLA-DRB4\nHLA-DQA1\tHLA-DRA\nDDT\tZNHIT1\nDDT\tMRPS12\nRAVER2\tSF3B1\nHLA-DMA\tHLA-DQB1\nHLA-DMA\tHLA-DRA\nHLA-DMA\tHLA-DRB1\nHLA-DMA\tHLA-DPA1\nHLA-DMA\tHLA-DQA1\nHLA-DMA\tHLA-DRB4\nHLA-DMA\tHLA-DPB1\nHLA-DMA\tHLA-DRB5\nRAB6A\tRAB6C\nC1orf160\tTCEB2\nC1orf160\tNDUFA13\n217679_x_at\tPDE4C\n217679_x_at\tDBT\n217679_x_at\tPGF\n217679_x_at\tCEP27\n217679_x_at\tZNF160\n217679_x_at\tFLJ12151\n217679_x_at\tSLC35E1\n217679_x_at\tKNS2\n217679_x_at\tFBXW12\n217679_x_at\tZNF611\nRPL21\tRPL26\nRPL21\tRPL36A\nRPL21\tRPL5\nRPL21\tRPL9\nRPL21\tRPL30\nRPL21\tRPS17\nRPL21\tTPT1\nRPL21\tRPS7\nRPL21\tRPL39\nRPL21\tRPS23\nRPL21\tRPL24\nRPL21\tRPS27A\nRPL21\tRPS3A\nRPL21\tU16\nRPL21\tRPL22\nRPL21\tRPS6\nRPL21\tRPL6\nRPL21\tRPL23\nRPL21\tRPL7\nRPL21\tRPS4X\nRPL21\tRPL4\nRPL21\tRPS24\nRPL21\tRPL34\nRPL21\tRPL31\nRPS13\tRPS20\nRPS13\tRPS27\nRPL18\tRPS2\nRPL18\tRPL18A\nRPS11\tRPS20\nNPTN\tRAB1A\nNPTN\tPRKAR1A\nMCM3AP\tOPHN1\nMCM3AP\tORC6L\nEIF4G1\tTHRAP5\n216412_x_at\tIGLV3-10\n216412_x_at\tIGLC2\nCXCR4\tLAPTM5\nCXCR4\tPRG1\nCXCR4\tHCLS1\nFAU\tRPS9\nFAU\tRPL35\nFAU\tRPS5\nFAU\tRPS16\nFAU\tRPL28\nRPL39\tRPL4\nRPL39\tRPS4X\nRPL39\tRPS20\nRPL39\tRPS15A\nRPL39\tRPS17\nRPL39\tRPS23\nIL2RG\tTRA@\nKIDINS220\tZFR\nKIDINS220\tSEPT7\nCOX4I1\tNDUFA7\nORC6L\tSLC25A16\nORC6L\tRPS11\nGBL\tGPX4\nATPIF1\tFIBP\nMGC2474\tTXNL2\nSEPT2\tSFRS11\nMT1F\tMT1L\nMT1F\tMT1X\nMT1F\tMT2A\nMT1F\tMT1G\nMT1F\tMT1H\nMT1F\tMT1M\nMT1L\tMT1M\nMT1L\tMT2A\nMT1L\tMT1X\nC11orf2\tFLJ14346\nATP6AP2\tPRKAR1A\nSTAT1\tTRIM22\nRPL35\tRPS9\nRPL35\tRPS5\nRPL35\tRPS16\nMT1E\tMT1L\nMT1E\tMT1F\nMT1E\tMT1G\nMT1E\tMT1X\nMT1E\tMT2A\nMT1E\tMT1M\nMT1E\tMT1H\nAKT1\tPGLS\nLOC56902\tMCM3AP\nLOC56902\tMGC2474\nHNRPM\tSERBP1\nCALD1\tPLEKHC1\nCALD1\tTPM1\nMT1M\tMT2A\nMT1M\tMT1X\nC17orf62\tIRF7\nKIAA1840\tPAPOLA\nDES\tTPM1\nMASP1\tPOLR2J\n212498_at\tTTC19\nHLA-DPB1\tHLA-DQB1\nHLA-DPB1\tHLA-DQA1\nHLA-DPB1\tHLA-DRB1\nHLA-DPB1\tHLA-DRB4\nHLA-DPB1\tHLA-DRB5\nFBXW12\tPRR11\nFBXW12\tRIOK3\nFBXW12\tPDE4C\nFBXW12\tFLJ42393\nFBXW12\tFLJ12151\nFBXW12\tPGF\nFBXW12\tSLC35E1\nFBXW12\tZNF611\nFBXW12\tLOC152719\nFBXW12\tKNS2\nFBXW12\tZNF160\nREEP5\tZMYND11\nPHIP\tZNF638\nRPS5\tRPS9\nWDR45\tZNF611\nMT1X\tMT2A\nCNOT2\tFLJ11021\nP4HB\tSEC61A1\nTMEM93\tUQCRFS1\nRPL37\tRPS15A\nRPL37\tRPS17\nRPL37\tRPS13\nRPL37\tRPL39\nACO2\tNDUFB2\nMT1G\tMT1L\nMT1G\tMT1M\nMT1G\tMT2A\nMT1G\tMT1H\nMT1G\tMT1X\nATP5J2\tUCRC\nRPL32\tRPL35A\nRPL32\tRPS15A\nRPL32\tRPS17\nRPL32\tRPL37\nGOLGA8A\tGOLGA8B\nCAST\tNFE2L2\nLOC645745\tMT1L\nLOC645745\tMT1F\nLOC645745\tMT2A\nLOC645745\tMT1M\nLOC645745\tMT1E\nLOC645745\tMT1X\nLOC645745\tMT1G\nLOC645745\tMT1H\nITGAV\tZMYND11\nCNN1\tMYH11\n208238_x_at\tRIOK3\nFLJ20294\tRPLP2\nFLJ20294\tRPS11\nFLJ20294\tRPL38\nFLJ20294\tH3F3A\nFLJ20294\tRPS20\nFLJ20294\tRPS19\nFLJ20294\tRPL27A\nFLJ20294\tRPL27\nSPCS2\tSRP9\n208120_x_at\tORC6L\n',
      },
      {
        kind: 'annotation',
        name: 'Co-expression KEGG pathways',
        fileName: 'NORMA_Human_coexpression_Annotation_KEGG.txt',
        text: 'Ribosome\tRPS5,RPS5,RPL35,FAU,RPL18,RPL21,RPL36,RPL12,RPLP2,RPS19,RPL27,RPL10A,LOC440055,RPS27,LOC388344,RPL27A,RPS10,RPS17,RPL28,LOC283412,RPL23A,RPL36A,RPL7,RPS2,U16,RPL35A,RPL18A,RPL13,RPL29,RPS15A,RPS3A,RPL14,RPL22,RPL34,RPS27A,RPL9,RPL6,RPL4,RPS4X,RPS9,RPL13A,RPL17,RPL5,MDS1,LOC402057\nOxidative phosphorylation\tNDUFC1,NDUFC1,ATP5J2,NDUFA10,NDUFA7,COX4I1,ATP6V0D1,NDUFB6,UCRC,SDHA,ATP6V0B,UQCRC1,NDUFB2,UQCRFS1,NDUFA2,NDUFV1,ATP6V0C,NDUFA3,NDUFA13,NDUFS7,COX5B,ATP5D,NDUFS3,NDUFS8,COX5A,NDUFB4,NDUFAB1,ATP5G3,CYC1,COX8A,RPA1\nAntigen processing and presentation\tHLA-DPB1,HLA-DPB1,TAPBP,HLA-DMA,HLA-DQA1,HLA-J,HLA-B,HLA-DRA,HLA-DMB,HLA-DRB5,CD74,HLA-DRB1,HLA-E,HLA-DRB4,HSP90AA1,C6orf12,HLA-DQB1,HLA-DPA1,IFI30,HLA-A,HLA-G,EIF3S3,HLA-C\nCell adhesion molecules (CAMs)\tITGAV,ITGAV,HLA-DPB1,HLA-DMA,HLA-DQA1,HLA-J,HLA-B,HLA-DRA,HLA-DMB,SELL,HLA-DRB5,HLA-DRB1,HLA-E,HLA-DRB4,CD2,C6orf12,HLA-DQB1,HLA-DPA1,HLA-A,SPN,HLA-G,ITGB2,HLA-C\nType I diabetes mellitus\tHLA-DPB1,HLA-DPB1,HLA-DMA,HLA-DQA1,HLA-J,HLA-B,HLA-DRA,HLA-DMB,HLA-DRB5,HLA-DRB1,HLA-E,HLA-DRB4,C6orf12,HLA-DQB1,HLA-DPA1,HLA-A,HLA-G,HLA-C\nFocal adhesion\tITGAV,ITGAV,AKT1,RAP1B,LAMC1,PGF,MYLK,COL6A3,COL1A1,COL6A1,ROCK1,COL4A2,COL4A1,BIRC2,COL3A1,COL1A2,CAV1,RAC2\nNatural killer cell mediated cytotoxicity\tHLA-J,HLA-J,CD48,HLA-B,HLA-E,C6orf12,IFNGR1,HLA-A,LCK,HLA-G,RAC2,ITGB2,HLA-C\nRegulation of actin cytoskeleton\tITGAV,ITGAV,MSN,MYH9,MYLK,ARPC1B,ROCK1,ARPC3,ARPC5,IQGAP1,RAC2,ITGB2\nCell Communication\tDES,DES,LAMC1,COL6A3,COL1A1,COL6A1,COL4A2,COL4A1,COL3A1,COL1A2,RPL38\nPyrimidine metabolism\tPOLR2L,POLR2L,POLR2J,HMHA1,POLR1B,208246_x_at,POLR2I,RPS15A,POLR2E\nECM-receptor interaction\tITGAV,ITGAV,LAMC1,COL6A3,COL1A1,COL6A1,COL4A2,COL4A1,COL3A1,COL1A2\nPurine metabolism\tGUK1,GUK1,POLR2L,POLR2J,HMHA1,POLR1B,POLR2I,RPS15A,POLR2E,PDE4C\nHematopoietic cell lineage\tCD37,CD37,HLA-DRA,CD3D,HLA-DRB5,HLA-DRB1,HLA-DRB4,CD2,CD7,IL7R\nCytokine-cytokine receptor interaction\tIL2RG,IL2RG,CXCR4,CCL5,IL10RA,LTB,IFNGR1,TGFB1,IL7R,IL6ST\nLeukocyte transendothelial migration\tRAP1B,RAP1B,CXCR4,NCF1,MSN,CYBA,ROCK1,MMP2,RAC2,ITGB2\nJak-STAT signaling pathway\tAKT1,AKT1,STAT1,IL2RG,RAVER2,IL10RA,IFNGR1,IL7R,IL6ST\nCell cycle\tORC6L,ORC6L,YWHAZ,217718_s_at,TGFB1,CDKN1B,SKP1A\nRNA polymerase\tPOLR2L,POLR2L,POLR2J,HMHA1,POLR1B,POLR2I,POLR2E\nApoptosis\tAKT1,AKT1,PRKAR1A,ENDOG,PRKAR2A,BIRC2,PRKACB\nCholera - Infection\tSEC61A1,SEC61A1,ATP6V0D1,ATP6V0B,ATP6V0C\nPathogenic Escherichia coli infection - EPEC\tHCLS1,HCLS1,YWHAZ,ROCK1,NCL,RPL38,ARPC5\nPathogenic Escherichia coli infection - EHEC\tHCLS1,HCLS1,YWHAZ,ROCK1,NCL,RPL38,ARPC5\nEpithelial cell signaling in Helicobacter pylori infection\tATP6V0D1,ATP6V0D1,CCL5,ATP6V0B,ATP6V0C\nWnt signaling pathway\tROCK1,ROCK1,SKP1A,CSNK1A1,RAC2,PRKACB\nPancreatic cancer\tAKT1,AKT1,STAT1,RAVER2,PGF,TGFB1,RAC2\nMAPK signaling pathway\tAKT1,AKT1,RAP1B,TGFB1,RAC2,PRKACB\nInsulin signaling pathway\tAKT1,AKT1,PRKAR1A,PRKAR2A,PRKACB\nProtein export\tSEC61A1,SEC61A1,SRP72,SRP9\nUbiquitin mediated proteolysis\tTCEB2,TCEB2,SKP1A,UBE2D2\nCitrate cycle (TCA cycle)\tACO2,ACO2,SDHA,MDH2,RPA1\nTGF-beta signaling pathway\tROCK1,ROCK1,TGFB1,SKP1A\nCalcium signaling pathway\tMYLK,MYLK,ATP2A3,PRKACB\nHedgehog signaling pathway\tCSNK1A1,CSNK1A1,PRKACB\nChronic myeloid leukemia\tAKT1,AKT1,TGFB1,CDKN1B\nAxon guidance\tCXCR4,CXCR4,ROCK1,RAC2\nToll-like receptor signaling pathway\tAKT1,AKT1,STAT1,CCL5\nTight junction\tAKT1,AKT1,MYH9,HCLS1\nGlycan structures - degradation\tPOPDC3,POPDC3,MAN2B1\nColorectal cancer\tAKT1,AKT1,TGFB1,RAC2\nB cell receptor signaling pathway\tAKT1,AKT1,CD79A,RAC2\nPrion disease\tNFE2L2,NFE2L2,LAMC1\nT cell receptor signaling pathway\tAKT1,AKT1,CD3D,LCK\nLong-term potentiation\tRAP1B,RAP1B,PRKACB\nAdherens junction\tIQGAP1,IQGAP1,RAC2\nGlycan structures - biosynthesis 2\tCOL6A3,COL6A3,OGT\nGnRH signaling pathway\tMMP2,MMP2,PRKACB\nAdipocytokine signaling pathway\tAKT1,AKT1,RAVER2\nBasal transcription factors\tTAF7,TAF7,GTF2I\nVEGF signaling pathway\tAKT1,AKT1,RAC2\nReductive carboxylate cycle (CO2 fixation)\tACO2,ACO2,MDH2\nGlyoxylate and dicarboxylate metabolism\tACO2,ACO2,MDH2\nGlutathione metabolism\tGPX1,GPX1,GPX4\nFc epsilon RI signaling pathway\tAKT1,AKT1,RAC2\nArachidonic acid metabolism\tGPX1,GPX1,GPX4\nmTOR signaling pathway\tAKT1,AKT1,PGF\nTyrosine metabolism\tECH1,ECH1,MIF\n',
      },
      {
        kind: 'annotation',
        name: 'Co-expression GO biological process',
        fileName: 'NORMA_Human_coexpression_Annotation_GO_BP.txt',
        text: 'protein biosynthesis\tRPS15,RPS15,RPL37,RPS5,RPL35,FAU,RPS11,RPS21,RPL18,RPS13,RPL21,RPL36,RPL12,RPLP2,RPS19,RPL27,RPL31,MRPS12,LOC440055,RPS27,LOC388344,RPL27A,RPS10,RPS17,RPL28,LOC283412,RPL19,RPS16,RPS14,NARS,RPL7,RPS6,RPS2,U16,NACA,RPL18A,RPL13,RPL29,RPS15A,RPS20,RPS3A,RPS18,RPL14,MRPL34,RPL22,RPL34,RPS27A,EEF1D,RPL15,RPL9,RPL6,RPL26,RPL4,RPS4X,RPS24,RPS9,RPL13A,RPL17,EEF1A1,RPL5,RPL23,MDS1,RPS7,LOC402057,RPS25\nimmune response\tHLA-DPB1,HLA-DPB1,TAPBP,IL2RG,HLA-DMA,HLA-DQA1,IL16,221651_x_at,IGLV3-10,ARL6IP2,HLA-B,CD164,HLA-DRA,HLA-DMB,TRIM22,HLA-DRB5,HLA-DRB1,IGL@,IGLC1,IGLV3-25,TRB@,HLA-DRB4,LTB,ARHGDIB,C6orf12,HLA-DQB1,HLA-DPA1,TRBV3-1,GPSM3,IGLC2,TRA@,214836_x_at,IGHV1-69,IGHA1,HLA-A,CD7,IGHG3,IGLJ3,IL7R,IL6ST,HLA-C,IGKC,IGHM\nsignal transduction\tAKT1,AKT1,STAT1,IL2RG,CXCR4,OPHN1,CCL5,PGF,INPP5D,PRKAR1A,CD164,RPS27,HLA-DRB5,CD74,HLA-DRB1,PRKAR2A,CD53,HLA-DRB4,LTB,HSP90AA1,OGT,ROCK1,IFNGR1,PTGES3,HMGB1,GTF2I,GNB2L1,CAP1,SPN,NPM1,IQGAP1,PAFAH1B1,EIF3S3,RAC2,COPS2,PRKACB,PDE4C\nantigen presentation, endogenous antigen\tHLA-J,HLA-J,RPLP2,221651_x_at,IGLV3-10,IGHD,HLA-B,CD74,IGL@,IGLC1,HLA-E,IGLV3-25,C6orf12,IGLC2,214836_x_at,IGHV1-69,IGHA1,HLA-F,HLA-A,IGHG3,IGLJ3,HLA-G,HLA-C,IGKC,IGHM\nantigen processing, endogenous antigen via ...\tCAST,CAST,HLA-J,RPLP2,221651_x_at,IGLV3-10,IGHD,HLA-B,IGL@,IGLC1,HLA-E,IGLV3-25,C6orf12,IGLC2,214836_x_at,IGHV1-69,IGHA1,HLA-F,HLA-A,IGHG3,IGLJ3,HLA-G,HLA-C,IGKC,IGHM\nregulation of transcription, DNA-dependent\tNFE2L2,NFE2L2,STAT1,MGC2474,LYSMD4,ZMYND11,ZNF160,CDC5L,TRIM22,SFPQ,IFI16,HCLS1,ATRX,HMGB1,CSDE1,SCAND1,RPL6,LOC342346,SUB1\nelectron transport\tMT1X,MT1X,MT1L,COX4I1,NCF1,SDHA,UQCRQ,UQCRC1,GPX4,UQCRFS1,NDUFV1,COX5B,NDUFS3,NDUFS8,COX5A,NDUFB4,CYC1,COX8A,RPA1\nprotein amino acid phosphorylation\tAKT1,AKT1,RAVER2,GMFG,PRKAR1A,PRKD2,MYLK,PRKAR2A,ROCK1,TGFB1,PCM1,U16,LCK,CSNK1A1,HNRPDL,RPL4,PRKACB\nantigen processing, exogenous antigen via M...\tHLA-DPB1,HLA-DPB1,HLA-DMA,HLA-DQA1,HLA-DRA,HLA-DMB,HLA-DRB5,HLA-DRB1,HLA-DRB4,HLA-DQB1,HLA-DPA1\nantigen presentation, exogenous antigen\tHLA-DPB1,HLA-DPB1,HLA-DMA,HLA-DQA1,HLA-DRA,HLA-DMB,HLA-DRB5,HLA-DRB1,HLA-DRB4,HLA-DQB1,HLA-DPA1\ncell adhesion\tITGAV,ITGAV,LAMC1,CCL5,FNBP4,ADRM1,CD164,TGFB1I1,SELL,DGCR6,COL6A1,PLEKHC1,CD2,ITGB2\nintracellular signaling cascade\tSTAT1,STAT1,RAVER2,INPP5D,PRKAR1A,PRKD2,NCF1,PRKAR2A,WSB1,HCLS1,HMHA1,ROCK1,LCK\ntranscription from RNA polymerase II promoter\tTARDBP,TARDBP,NFE2L2,STAT1,POLR2J,TGFB1I1,TCEA1,HMHA1,POLR2I,BTF3,COPS2,POLR2E\ncell motility\tCALD1,CALD1,CAPZA1,CCL5,TPM1,MSN,ACTR3,ARPC1B,ARHGDIB,ARPC3,ARPC5,PAFAH1B1\ndetection of pest, pathogen or parasite\tHLA-DPB1,HLA-DPB1,HLA-DMA,HLA-J,HLA-DMB,HLA-DRB5,HLA-DRB1,HLA-DRB4,HLA-G\nnuclear mRNA splicing, via spliceosome\tHNRPM,HNRPM,SFRS10,HNRPH3,SFPQ,HIATL1,RPL35A,SF3B1,HNRPA2B1,HNRPA1\nmitochondrial electron transport, NADH to u...\tNDUFC1,NDUFC1,NDUFB6,NDUFB2,NDUFV1,NDUFS7,NDUFS3,NDUFS8,NDUFB4\ndevelopment\tCECR1,CECR1,CLPTM1,CD164,ARHGDIB,COL6A3,MYH11,ITGB2,DDX1\nphosphate transport\tCOL6A3,COL6A3,COL1A1,COL6A1,COL4A2,COL4A1,COL3A1,COL1A2\nantigen presentation\tHLA-J,HLA-J,HLA-B,HLA-E,C6orf12,HLA-F,HLA-A,HLA-G,HLA-C\ntransport\tG3BP,G3BP,SLC25A16,UCP2,SLC25A11,TMED5,SLC25A3,ATP2A3\ncell proliferation\tRPS21,RPS21,PGF,ZMYND11,RPS27,CD74,IFI16,MIF,ISG20\nanti-apoptosis\tAKT1,AKT1,FAIM3,SON,HMGB1,HSP90B1,NPM1,TPT1,PRDX2\ncell surface receptor linked signal transdu...\tCD3D,CD3D,CD2,CD79A,BIRC2,MIF,CSNK1A1,IL7R,IL6ST\ngeneration of precursor metabolites and energy\tACO2,ACO2,ATPIF1,COX4I1,ECH1,UQCR,SLC25A3,COX8A\nprotein complex assembly\tTAPBP,TAPBP,CAPZA1,GPAA1,LAMC1,TCEB2,CD3D,CD74\nmRNA processing\tSFPQ,SFPQ,SRP46,DHX15,HNRPR,SFRS11,SFRS5,RNPC2\npositive regulation of cell proliferation\tSSR1,SSR1,CAPNS1,PGF,RPL31,HCLS1,TGFB1,NAP1L1\nRNA splicing\tHNRPH3,HNRPH3,SFPQ,DHX15,PPP2CA,ZNF638,SFRS11\ntranscription\tLYSMD4,LYSMD4,POLR2J,ZNF160,SFPQ,POLR1B,PCM1\nregulation of translational initiation\tEIF4G1,EIF4G1,EIF4A2,EIF4G2,EIF5,EIF3S3,DDX1\npathogenesis\tHLA-DPB1,HLA-DPB1,HLA-DRB5,HLA-DRB1,HLA-DRB4\nproton transport\tATP5J2,ATP5J2,ATP6V0D1,UCP2,ATP6V0B,ATP6V0C\nRNA processing\tHNRPU,HNRPU,RBPMS,DDX17,HNRPH1,HNRPDL,RNPC2\nregulation of transcription from RNA polyme...\tTHRAP5,THRAP5,PRKAR1A,HMGB1,LOC342346,SUB1\nprotein transport\tRAB1A,RAB1A,RAB6A,RAB6C,TLOC1,HSP90B1,COPB\nsmall GTPase mediated signal transduction\tRAP1B,RAP1B,RGS19,RAB1A,RAB6A,RAB6C,RPL29\nresponse to virus\tCCL5,CCL5,IRF7,TRIM22,IFI16,IFNGR1,ISG20\ninflammatory response\tCCL5,CCL5,IRF7,MEFV,ALOX5AP,TGFB1,ITGB2\nactin cytoskeleton organization and biogenesis\tARHGDIB,ARHGDIB,PLEKHC1,DST,ROCK1,ARPC5\nprotein folding\tPPIB,PPIB,HSP90AA1,HSP90B1,EIF3S3,KTN1\ninduction of apoptosis\tCD2,CD2,PPP2CA,BCLAF1,LCK,RPS3A,SCAND1\nskeletal development\tCNOT2,CNOT2,COL1A1,TGFB1,COL1A2,MYH11\nregulation of progression through cell cycle\tSTAT1,STAT1,PGF,ERH,PPP2CA,LCK,SCAND1\nnegative regulation of cell proliferation\tCD164,CD164,TGFB1I1,TGFB1,CDKN1B,NPM1\ncellular defense response\tHLA-J,HLA-J,CCL5,NCF1,FAIM3,SPN,HLA-G\ntranslational elongation\tRPLP2,RPLP2,TUFM,EEF1B2,EEF1D,EEF1A1\nmuscle development\tTAGLN,TAGLN,LYSMD4,SMTN,COL6A3,MBNL1\nATP synthesis coupled proton transport\tATP6V0B,ATP6V0B,ATP6V0C,ATP5D,ATP5G3\nproteolysis\tCAST,CAST,MASP1,YME1L1,MMP2,IGHG3\nmicrotubule-based movement\t220725_x_at,220725_x_at,KNS2,KTN1\nmetabolism\tACO2,ACO2,ECH1,POPDC3,IGLC2,DIP2A\napoptosis\tAD7C-NTP,AD7C-NTP,FIS1,ITGB2,CROP\namino acid transport\tSLC38A2,SLC38A2,218041_x_at,RPL24\nresponse to unfolded protein\tHSP90AA1,HSP90AA1,HSP90B1,EIF3S3\nintracellular protein transport\tVDP,VDP,NAPA,CLTC,CD74,NPM1,COPE\ntranscription initiation from RNA polymeras...\tTHRAP5,THRAP5,IRF7,THRAP1,GTF2I\nresponse to oxidative stress\tCCL5,CCL5,GPX1,GPX4,CSDE1,PRDX2\nnucleobase, nucleoside, nucleotide and nucl...\tNDUFA10,NDUFA10,ERH,208246_x_at\ncarbohydrate metabolism\tLOC91316,LOC91316,MAN2B1,AKR7A2\nprotein modification\tSUMO2,SUMO2,RPN1,MAN2B1,RPS27A\nubiquitin-dependent protein catabolism\tUSP34,USP34,UBE4A,PSMB9,PSMB8\ndefense response\tCD48,CD48,PTPRCAP,HLA-B,CD79A\nDNA replication\tMCM3AP,MCM3AP,NARS,NAP1L1,SET\nandrogen receptor signaling pathway\tTHRAP5,THRAP5,THRAP1,TGFB1I1\nprotein targeting to mitochondrion\tTIMM8B,TIMM8B,TOMM20,TIMM13\noxygen transport\tSLC38A2,SLC38A2,218041_x_at\nG-protein coupled receptor protein signalin...\tAKT1,AKT1,RGS19,CXCR4,FNBP4\nnervous system development\tOPHN1,OPHN1,MBNL1,PAFAH1B1\nmitochondrial transport\tUCP2,UCP2,HSP90AA1,EIF3S3\nembryo implantation\tLOC283412,LOC283412,RPL29\nDNA repair\tSFPQ,SFPQ,ATRX,HMGB1,RPA1\nprotein refolding\tHSP90AA1,HSP90AA1,EIF3S3\npositive regulation of nitric oxide biosynt...\tHSP90AA1,HSP90AA1,EIF3S3\nlipid metabolism\tPAFAH1B1,PAFAH1B1,ATP8B1\ncell cycle arrest\tEIF4G2,EIF4G2,DST,CDKN1B\ncell cycle\tCUL4B,CUL4B,RPS27A,SEPT7\nprotein ubiquitination\tUBE4A,UBE4A,PCNP,RPS27A\nprotein targeting\tTOMM20,TOMM20,SRP9,COPB\nnucleosome assembly\tNAP1L1,NAP1L1,SET,H3F3A\nnegative regulation of cell adhesion\tCD164,CD164,ARHGDIB,SPN\ncell-cell signaling\tCCL5,CCL5,PGF,LTB,ITGB2\npositive regulation of transcription\tTGFB1I1,TGFB1I1,RPS27A\nnegative regulation of transcription\tIRF7,IRF7,SUMO1,BCLAF1\nhemopoiesis\tCD164,CD164,ZNF160,LCK\ncellular defense response (sensu Vertebrata)\tIGHA1,IGHA1,IGHG3,IGHM\nretrograde vesicle-mediated transport, Golg...\tTAPBP,TAPBP,COPE,COPB\nprotein import into nucleus\tMCM3AP,MCM3AP,NDUFA13\npositive regulation of transcription, DNA-d...\tTGFB1I1,TGFB1I1,TGFB1\nnegative regulation of cell growth\tPPP2CA,PPP2CA,NDUFA13\nintegrin-mediated signaling pathway\tITGAV,ITGAV,DST,ITGB2\ncotranslational protein targeting to membrane\tSSR1,SSR1,TLOC1,TRAM1\nRho protein signal transduction\tARHGDIB,ARHGDIB,ROCK1\nsensory perception of sound\tTIMM8B,TIMM8B,TIMM13\nregulation of translation\tPPP2CA,PPP2CA,EEF1A1\nregulation of transcription\tPOLR2L,POLR2L,PPP2CA\nprostaglandin biosynthesis\tCD74,CD74,PTGES3,MIF\nmitosis\tTARDBP,TARDBP,CORO1A\ninsulin receptor signaling pathway\tPHIP,PHIP,AKT1,MYH11\nDNA recombination\tATRX,ATRX,HMGB1,RPA1\ntricarboxylic acid cycle\tACO2,ACO2,SDHA,RPA1\nnegative regulation of transcription, DNA-d...\tRPS14,RPS14,NDUFA13\nintra-Golgi vesicle-mediated transport\tNAPA,NAPA,COPE,COPB\ncell death\tEIF4G2,EIF4G2,TGFB1\nprotein amino acid autophosphorylation\tRPS2,RPS2,U16,RPL4\ncirculation\tRPL31,RPL31,COL3A1\ncalcium ion transport\tATP2A3,ATP2A3,TPT1\naxon guidance\tOPHN1,OPHN1,RPS27A\nER-associated protein catabolism\tRPL27,RPL27,RPS27A\nregulation of actin filament polymerization\tARPC3,ARPC3,ARPC5\npositive regulation of I-kappaB kinase/NF-k...\tBIRC2,BIRC2,EEF1D\norgan morphogenesis\tDGCR6,DGCR6,TGFB1\nnegative regulation of transcription from R...\tIRF7,IRF7,ZMYND11\nmRNA splice site selection\tSRP46,SRP46,SFRS5\nerythrocyte differentiation\tRPS19,RPS19,HCLS1\ncytokinesis\tSEPT2,SEPT2,SEPT7\nchromosome segregation\tARL8B,ARL8B,RIOK3\ncell-matrix adhesion\tITGAV,ITGAV,ITGB2\nactivation of NF-kappaB transcription factor\tU16,U16,NPM1,RPL4\nT cell activation\tCD3D,CD3D,CD2,CD7\nubiquitin cycle\tTPT1,TPT1,UBE2D2\nregulation of cell adhesion\tNPTN,NPTN,PPP2CA\ncell growth\tTGFB1,TGFB1,DDX5\nangiogenesis\tCAST,CAST,ATPIF1\ntranslational initiation\tEIF5,EIF5,RPS3A\ntransforming growth factor beta receptor si...\tFNTA,FNTA,TGFB1\nresponse to drug\tRAB6C,RAB6C,LCK\nregulation of apoptosis\tTPT1,TPT1,PRDX2\nchromosome organization and biogenesis (sen...\tATRX,ATRX,H3F3A\ncaspase activation\tSTAT1,STAT1,LCK\nantimicrobial humoral response (sensu Verte...\tIL7R,IL7R,ITGB2\nI-kappaB kinase/NF-kappaB cascade\tPHIP,PHIP,STAT1\ntransmembrane receptor protein tyrosine kin...\tCD7,CD7,COL1A2\nresponse to stress\tNPM1,NPM1,CROP\nresponse to metal ion\tMT1X,MT1X,MT1L\ncalcium ion homeostasis\tCCL5,CCL5,TPT1\nregulation of macrophage activation\tCD74,CD74,MIF\nnucleocytoplasmic transport\tNPM1,NPM1,SET\nnegative regulation of apoptosis\tCD74,CD74,MIF\nmuscle contraction\tDES,DES,CALD1\nestablishment and/or maintenance of cell po...\tCAP1,CAP1,SPN\nchemotaxis\tCCL5,CCL5,SPN\nRas protein signal transduction\tG3BP,G3BP,LCK\nregulation of heart contraction\tDES,DES,TPM1\nactivation of MAPKK activity\tU16,U16,RPL4\nJNK cascade\tU16,U16,RPL4\ncytoskeleton organization and biogenesis\tDES,DES,DST\n',
      },
      {
        kind: 'annotation',
        name: 'Co-expression GO molecular function',
        fileName: 'NORMA_Human_coexpression_Annotation_GO_MF.txt',
        text: "protein binding\tTARDBP,TARDBP,ITGAV,CAST,RPS15,PHIP,NFE2L2,OAZ1,STAT1,SEPT2,TXNL2,ORC6L,IL2RG,RGS19,EIF4G1,RPS13,C1orf160,RAB6A,HLA-DMA,ZNHIT1,GPAA1,CUL4B,HLA-J,KPNB1,ARF1,IL16,CD48,CBX3,G3BP,SUMO2,ACTA2,TCEB2,EIF3S6,MT1H,RPS19,INPP5D,RAB6C,RNF11,PRKAR1A,NCF1,ZMYND11,HNRPK,SUMO1,TGFB1I1,CDC5L,KNS2,RBPMS,CAB39,SFPQ,CD3D,NAPA,ACTR3,CLTC,TTC3,EIF4A2,IFI16,ARL6IP5,EEF1B2,HCLS1,PSMB8,HMHA1,HSP90AA1,HNRPH1,SRP46,EIF4G2,FIS1,PLEKHC1,CD2,OGT,TAF7,217718_s_at,NARS,C6orf12,TGFB1,CDKN1B,NDUFA13,GPSM3,BCLAF1,HSP90B1,SERBP1,GTF2I,RPS6,U16,BIRC2,RPL35A,HLA-A,RPS15A,EXOSC4,RPS20,RPL38,ACTG2,CAV1,ACTR2,SKP1A,ACTR10,MIF,PCNP,RPS3A,RPL14,HLA-G,IL7R,AURKAIP1,FIBP,EIF3S3,HNRPDL,RPS27A,RAC2,ITGB2,COPS2,XPO1,SIAHBP1,POLR2E,NAP1L1,RPL4,SFRS5,HNRPA2B1,EEF1A1,UBE2D2,SEPT7,COPB,ATP5G3,RPL5,RPS7,RPA1,HNRPA1,RPL24\nRNA binding\tTARDBP,TARDBP,RPS5,HNRPM,LOC56902,EIF4G1,RPL18,RPL21,SFRS10,RPLP2,G3BP,HNRPH3,HNRPU,DDX42,RPS19,HNRPK,RBPMS,LOC440055,RPS27,LOC388344,SFPQ,DDX17,RPL27A,RPS10,RPS17,EIF4A2,RPL28,LOC283412,RPS14,HNRPH1,SRP46,EIF4G2,HNRPR,NCL,RPL7,HSP90B1,SRP9,HNRPA3P1,RPS2,U16,DDX5,ZNF638,RPL18A,RPL13,RPL29,SFRS11,DDX50,NPM1,RPS18,RPL14,PAPOLA,RPL22,MATR3,HNRPDL,RPL34,SCAND1,RPL9,RPL6,SIAHBP1,RPL4,RPS4X,SFRS5,SFRS3,HNRPA2B1,RPS9,RNPC2,RPL5,MDS1,LOC402057,HNRPA1\nstructural constituent of ribosome\tRPS15,RPS15,RPL37,RPS5,RPL35,FAU,RPS11,RPS21,RPL18,RPS13,RPL21,RPL36,RPL12,RPLP2,RPS19,RPL27,RPL31,MRPS12,LOC440055,RPS27,LOC388344,RPL27A,RPS10,RPS17,RPL28,LOC283412,RPL19,RPS16,RPS14,RPL7,RPS6,RPS2,U16,RPL18A,RPL13,RPL29,RPS15A,RPS20,RPS3A,RPS18,RPL14,MRPL34,RPL22,RPL34,RPS27A,RPS23,RPL15,RPL9,RPL6,RPL26,RPL4,RPS4X,RPS24,RPS9,RPL13A,RPL17,RPL5,RPL23,MDS1,RPS7,LOC402057,RPS25\nnucleotide binding\tTARDBP,TARDBP,HNRPM,RAP1B,MCM3AP,RAB1A,RAB6A,RAVER2,SFRS10,G3BP,HNRPH3,ACTA2,DDX42,RAB6C,PRKD2,RBPMS,YME1L1,SFPQ,DDX17,EIF4A2,HSP90AA1,HNRPH1,SRP46,NARS,HNRPR,NCL,PCM1,HSP90B1,HNRPA3P1,U16,DDX5,ACTG2,SFRS11,DDX50,CSNK1A1,EIF3S3,MATR3,HNRPDL,DDX3X,SIAHBP1,RPL4,SFRS5,HNRPA2B1,EEF1A1,RNPC2,SEPT7,PRKACB,HNRPA1\nnucleic acid binding\tTARDBP,TARDBP,ZFR,HNRPM,LOC56902,MGC2474,RPS11,SFRS10,G3BP,HNRPH3,ZNF9,DDX42,HNRPK,ZNF160,MRPS12,SFPQ,DDX17,R3HCC1,EIF4A2,IFI16,HNRPH1,SRP46,NARS,ZNF611,SON,HNRPR,DDX5,SFRS11,DDX50,ZNF552,NPM1,MBNL1,MATR3,HNRPDL,DDX3X,SIAHBP1,SFRS5\nATP binding\tNDUFA10,NDUFA10,AKT1,RAVER2,ACTA2,DDX42,PRKD2,MYH9,220725_x_at,YME1L1,DDX17,EIF4A2,HSP90AA1,208246_x_at,DHX15,NARS,ROCK1,NDUFA13,PCM1,HSP90B1,ATP2A3,RPS2,U16,DDX5,LCK,MYH11,ACTG2,DDX50,CSNK1A1,EIF3S3,DDX3X,RPL4,PRKACB\nreceptor activity\tITGAV,ITGAV,HNRPM,IL2RG,CXCR4,THRAP5,PTPRCAP,RPL27,IL10RA,THRAP1,TLOC1,HLA-DRB5,HLA-DRB1,HLA-DRB4,CD2,IFNGR1,TRBV3-1,IGHA1,U16,CD7,IGHG3,IL7R,IL6ST,RPL4,EEF1A1,IGHM,TRAM1\nMHC class I receptor activity\tHLA-J,HLA-J,RPLP2,221651_x_at,IGLV3-10,IGHD,HLA-B,IGL@,IGLC1,HLA-E,IGLV3-25,C6orf12,IGLC2,214836_x_at,IGHV1-69,IGHA1,HLA-F,HLA-A,IGHG3,IGLJ3,HLA-G,HLA-C,IGKC,IGHM\nzinc ion binding\tCAST,CAST,ZFR,MT1E,MT1F,KPNB1,MSRB2,TIMM8B,ZNF9,DDX42,RNF11,PRKD2,SDHA,ZMYND11,ZNF160,MEFV,RPS27,TTC3,ZNF611,TIMM13,POLR2I,MMP2,ZNF552,MBNL1,MATR3\nbinding\tSRP72,SRP72,VDP,CAPZA1,TTC19,KPNB1,SLC25A16,MSN,UCP2,KNS2,CAB39,RGPD5,NAPA,SLC25A11,TTC3,SLC25A3,ALOX5AP,HIATL1,OGT,CHCHD2,SF3B1,COPS2,COPE,COPB\nDNA binding\tMCM3AP,MCM3AP,HNRPU,RNF11,IRF7,SDHA,POLR2L,POLR2J,CDC5L,SFPQ,POLR1B,SON,HMGB1,PCM1,BCLAF1,CSDE1,RPL6,LOC342346,SUB1,LOC58486,H3F3A\nNADH dehydrogenase (ubiquinone) activity\tNDUFC1,NDUFC1,NDUFA10,NDUFA7,NDUFB6,NDUFB2,NDUFA2,NDUFV1,NDUFA3,NDUFA13,NDUFS7,NDUFS3,NDUFS8,NDUFB4,NDUFAB1\nMHC class II receptor activity\tHLA-DPB1,HLA-DPB1,HLA-DMA,HLA-DQA1,HLA-DRA,HLA-DMB,HLA-DRB5,HLA-DRB1,HLA-DRB4,HLA-DQB1,HLA-DPA1,HLA-C\nelectron carrier activity\tP4HB,P4HB,MT1X,MT1L,NCF1,UQCR,CYBA,FLJ14346,NDUFS3,NDUFS8,COX5A,ETFB,KTN1,AKR7A2,CYC1\nGTPase activity\tSEPT2,SEPT2,RAB6A,ARF1,RAB6C,NCF1,ARL6IP2,ARF5,ARL8B,RPL29,EIF5,RAC2,EEF1A1\nantigen binding\tIGL@,IGL@,IGLC1,IGLV3-25,IGLC2,IGHV1-69,IGHA1,IGHG3,IGLJ3,IL7R,IGKC,IGHM\ntransferase activity\tFNTA,FNTA,PRKD2,POLR2J,POLR1B,COL6A3,NARS,RPS2,U16,CSNK1A1,RPL4,PRKACB\ntranscription factor activity\tTARDBP,TARDBP,NFE2L2,STAT1,LYSMD4,MSRB2,TRIM22,HCLS1,GTF2I,SCAND1,MDS1\nmetal ion binding\tMT1X,MT1X,ZFR,MT1M,MT1L,PRKD2,ZNF160,TTC3,ZNF611,ZNF552,MATR3,NDUFS8\nstructural molecule activity\tDES,DES,FNBP4,CORO1A,NPEPPS,CLTC,COL6A3,RPL38,CAV1,MATR3,SEPT7\nGTP binding\tRAP1B,RAP1B,RAB1A,RAB6A,RAB6C,NCF1,ARL6IP2,ARL8B,EEF1A1,SEPT7\nextracellular matrix structural constituent\tLAMC1,LAMC1,FBLN1,COL1A1,COL6A1,COL4A2,COL4A1,COL3A1,COL1A2\nstructural constituent of cytoskeleton\tDES,DES,ADD3,ACTA2,TPM1,MSN,ARPC1B,DST,ACTG2,ARPC3,ARPC5\nhydrolase activity\tDDX42,DDX42,PPP1CA,DDX17,EIF4A2,PPP2CA,DDX5,DDX50,DDX3X\nunfolded protein binding\tTAPBP,TAPBP,PPIB,HSP90AA1,PTGES3,HSP90B1,NPM1,EIF3S3\ntransporter activity\tG3BP,G3BP,UCP2,ATP6V0B,SLC25A11,SLC25A3,ATP5D,ATP5G3\nkinase activity\tGUK1,GUK1,MYLK,PRKAR2A,WSB1,208246_x_at,ROCK1,CDKN1B\niron ion binding\tACO2,ACO2,MT1X,MT1L,PPP1CA,NDUFV1,PPP2CA,NDUFS8,CYC1\nDNA-directed RNA polymerase activity\tPOLR2L,POLR2L,POLR2J,HMHA1,POLR1B,PCM1,POLR2I,POLR2E\nATP-dependent helicase activity\tDDX42,DDX42,DDX17,EIF4A2,DHX15,DDX5,DDX50,DDX3X,DDX1\nprotein serine/threonine kinase activity\tAKT1,AKT1,PRKD2,ROCK1,RPS2,U16,CSNK1A1,RPL4,PRKACB\ncalcium ion binding\tMASP1,MASP1,SPARCL1,STAT1,FBLN1,HSP90B1,IGHG3,TPT1\nactin binding\tCNN1,CNN1,CALD1,CAPZA1,GMFG,SMTN,MEFV,NPEPPS,FXYD5\nprotein homodimerization activity\tATPIF1,ATPIF1,HSP90AA1,U16,NPM1,EIF3S3,RPL4\nsignal transducer activity\tSTAT1,STAT1,RGS19,BIRC2,HNRPDL,EEF1D,COPS2\noxidoreductase activity\tNDUFA10,NDUFA10,GPX1,MDH2,GPX4,IGLC2,PRDX2\ntranscription coactivator activity\tTGFB1I1,TGFB1I1,TAF7,NPM1,LOC342346,SUB1\ntranslation initiation factor activity\tEIF4G1,EIF4G1,EIF4A2,EIF4G2,EIF5,EIF3S3\nprotein-tyrosine kinase activity\tRAVER2,RAVER2,PCM1,U16,LCK,HNRPDL,RPL4\ncatalytic activity\tKIAA1840,KIAA1840,LOC91316,DIP2A,PDE4C\nhydrogen-transporting ATPase activity, rota...\tATP6V0B,ATP6V0B,ATP6V0C,ATP5D,ATP5G3\nhydrogen-transporting ATP synthase activity...\tATP6V0B,ATP6V0B,ATP6V0C,ATP5D,ATP5G3\nhematopoietin/interferon-class (D200-domain...\tSTAT1,STAT1,IL2RG,IL10RA,IFNGR1,IL7R\noxidoreductase activity, acting on NADH or ...\tNDUFV1,NDUFV1,NDUFS7,NDUFS3,NDUFS8\nsingle-stranded DNA binding\tHNRPDL,HNRPDL,LOC342346,SUB1,RPA1\namino acid-polyamine transporter activity\tSLC38A2,SLC38A2,218041_x_at,RPL24\nreceptor binding\tCCL5,CCL5,ARPP-19,MSN,LTB,GNB2L1\ncalmodulin binding\tCALD1,CALD1,MARCKS,ATPIF1,IQGAP1\ncytochrome-c oxidase activity\tCOX4I1,COX4I1,COX5B,COX5A,COX8A\nubiquinol-cytochrome-c reductase activity\tUCRC,UCRC,UQCRQ,UQCRC1,UQCRFS1\nprotein transporter activity\tVDP,VDP,KPNB1,TLOC1,AP2S1,COPE\ntranslation elongation factor activity\tTUFM,TUFM,EEF1B2,EEF1D,EEF1A1\npeptide antigen binding\tTAPBP,TAPBP,TRB@,TRBV3-1,TRA@\nmicrotubule motor activity\t220725_x_at,220725_x_at,KNS2\nphosphotransferase activity, alcohol group ...\tNDUFA10,NDUFA10,208246_x_at\noxygen transporter activity\tSLC38A2,SLC38A2,218041_x_at\nRNA helicase activity\tDDX17,DDX17,DHX15,DDX5,DDX1\ngrowth factor activity\tCECR1,CECR1,GMFG,PGF,TGFB1\nRNA splicing factor activity, transesterifi...\tSFRS10,SFRS10,HIATL1,SF3B1\nheparin binding\tLOC283412,LOC283412,RPL29\ndouble-stranded DNA binding\tIFI16,IFI16,ZNF638,HNRPDL\nRNA polymerase II transcription mediator ac...\tCNOT2,CNOT2,THRAP5,THRAP1\nnitric-oxide synthase regulator activity\tHSP90AA1,HSP90AA1,EIF3S3\nisomerase activity\tP4HB,P4HB,ECH1,PPIB,KTN1\nTPR domain binding\tHSP90AA1,HSP90AA1,EIF3S3\nubiquitin-protein ligase activity\tUBE4A,UBE4A,TPT1,UBE2D2\nprotein domain specific binding\tYWHAZ,YWHAZ,217718_s_at\ncAMP-dependent protein kinase regulator act...\tPRKAR1A,PRKAR1A,PRKAR2A\ntranscriptional repressor activity\tIRF7,IRF7,IFI16,BCLAF1\nprotein kinase binding\tU16,U16,LCK,ITGB2,RPL4\ngeneral RNA polymerase II transcription fac...\tTCEA1,TCEA1,TAF7,GTF2I\nMHC protein binding\tTRB@,TRB@,TRBV3-1,TRA@\nATP-dependent RNA helicase activity\tG3BP,G3BP,RPL35A,DDX3X\nprotein heterodimerization activity\tCD3D,CD3D,PPP2CA,NPM1\npeptidase activity\tMASP1,MASP1,ZNF9,CLPP\nidentical protein binding\tAKT1,AKT1,CD74,SCAND1\nvitamin D receptor binding\tTHRAP5,THRAP5,THRAP1\nthyroid hormone receptor binding\tTHRAP5,THRAP5,THRAP1\nserine-type endopeptidase inhibitor activity\tNFE2L2,NFE2L2,COL6A3\nsequence-specific DNA binding\tNFE2L2,NFE2L2,LYSMD4\nprotein dimerization activity\tNFE2L2,NFE2L2,POLR2J\nmotor activity\tMYH9,MYH9,KNS2,MYH11\ncopper ion binding\tMT1E,MT1E,MT1F,DDX42\ncadmium ion binding\tMT1E,MT1E,MT1F,DDX42\ntransmembrane receptor activity\tCD3D,CD3D,CD79A,SPN\ntranslation factor activity, nucleic acid b...\tEIF3S3,EIF3S3,EEF1D\ntranscriptional activator activity\tTHRAP5,THRAP5,TGFB1\n3'-5'-exoribonuclease activity\tEXOSC4,EXOSC4,ISG20\ntropomyosin binding\tCALD1,CALD1,LMOD1\ntranscription factor binding\tYWHAZ,YWHAZ,HMGB1\nthreonine endopeptidase activity\tPSMB9,PSMB9,PSMB8\nserine-type endopeptidase activity\tMASP1,MASP1,IGHG3\nperoxidase activity\tCSDE1,CSDE1,PRDX2\nenzyme activator activity\tGMFG,GMFG,ALOX5AP\nantioxidant activity\tRPL27,RPL27,PRDX2\nactin filament binding\tMARCKS,MARCKS,DST\nacetylglucosaminyltransferase activity\tCOL6A3,COL6A3,OGT\nATPase binding\tATPIF1,ATPIF1,LCK\ntranscription regulator activity\tRPL7,RPL7,RPS27A\nsugar binding\tMASP1,MASP1,SELL\nligase activity\tNARS,NARS,UBE2D2\ncytokine binding\tCD74,CD74,IFNGR1\naminopeptidase activity\tCAST,CAST,NPEPPS\nprotein phosphatase inhibitor activity\tPSMB9,PSMB9,SET\nsuccinate dehydrogenase activity\tSDHA,SDHA,RPA1\nstructural constituent of muscle\tTPM1,TPM1,SMTN\nprotein disulfide oxidoreductase activity\tP4HB,P4HB,KTN1\nprotein disulfide isomerase activity\tP4HB,P4HB,KTN1\nornithine decarboxylase inhibitor activity\tOAZ2,OAZ2,OAZ1\nglutathione peroxidase activity\tGPX1,GPX1,GPX4\ndouble-stranded RNA binding\tSON,SON,MBNL1\nCD4 receptor binding\tLCK,LCK,SPG21\nMAP kinase kinase kinase activity\tU16,U16,RPL4\nprotein C-terminus binding\tDST,DST,LCK\n",
      },
      {
        kind: 'annotation',
        name: 'Co-expression GO cellular component',
        fileName: 'NORMA_Human_coexpression_Annotation_GO_CC.txt',
        text: 'integral to membrane\tSPCS2,SPCS2,ITGAV,CAST,REEP5,HLA-DPB1,SSR1,C11orf2,TAPBP,IL2RG,CXCR4,HLA-J,RPLP2,CD37,SLC25A16,221651_x_at,RPN1,IGLV3-10,FNBP4,UCP2,AD7C-NTP,ADRM1,IGHD,HLA-B,HLA-DMB,TLOC1,ATP6V0B,HLA-DRB5,SLC25A11,CD74,HLA-DRB1,IGL@,TMED5,IGLC1,HLA-E,SLC25A3,IGLV3-25,TRB@,ARL6IP5,HLA-DRB4,TRBC1,LAPTM5,CD2,C6orf12,IFNGR1,HLA-DQB1,FXYD5,POPDC3,ATP6V0C,TRBV3-1,IGLC2,214836_x_at,IGHV1-69,IGHA1,U16,HLA-F,HLA-A,CD7,CAV1,IGHG3,215176_x_at,IGLJ3,HLA-G,IL7R,ITGB2,IL6ST,RPL4,HLA-C,IGKC,IGHM,TRAM1,RPA1\nmembrane\tHLA-DPB1,HLA-DPB1,SLC38A2,RAP1B,VDP,IL2RG,CXCR4,RAB1A,NPTN,RAB6A,HLA-DMA,HLA-DQA1,ADD3,SLC25A16,PGF,IL10RA,RAB6C,221651_x_at,MSN,IGLV3-10,RPL31,UCP2,IGHD,218041_x_at,HLA-DRA,HLA-DMB,YME1L1,SELL,ATP6V0B,HLA-DRB5,SLC25A11,HLA-DRB1,IGL@,IGLC1,SLC25A3,IGLV3-25,TRB@,HLA-DRB4,TRBC1,LTB,COL6A3,IFNGR1,HLA-DQB1,HLA-DPA1,CD79A,FXYD5,ATP6V0C,PPP2CA,TRBV3-1,IGLC2,ATP2A3,214836_x_at,IGHV1-69,IGHA1,IGHG3,215176_x_at,IGLJ3,IL7R,COPE,HLA-C,IGKC,IGHM,ATP5G3,RPA1,RPL24\nnucleus\tTARDBP,TARDBP,CNOT2,NFE2L2,ZFR,STAT1,SEPT2,MCM3AP,TINP1,LYSMD4,KPNB1,SFRS10,CBX3,THRAP5,G3BP,HNRPH3,IRF7,THRAP1,ZMYND11,POLR2J,ZNF160,MEFV,CDC5L,TRIM22,DDX17,IFI16,HCLS1,ATRX,HNRPH1,POLR1B,OGT,DHX15,ZNF611,MORF4L1,HNRPR,HMGB1,PPP2CA,CDKN1B,PCM1,BCLAF1,TIMM13,SRP9,RPS6,DDX5,ZNF638,RPL35A,SFRS11,PCNP,DDX50,ZNF552,RPS3A,NPM1,MBNL1,PAPOLA,AURKAIP1,FIBP,SH3BGRL,MATR3,RPS27A,SCAND1,DDX3X,SET,HNRPA2B1,RNPC2,SEPT7,H3F3A\ncytoplasm\tAKT1,AKT1,MT1E,FNTA,STAT1,MT1F,SEPT2,VDP,EIF4G1,KPNB1,DDX42,IRF7,MSN,ARL8B,CD3D,PRKAR2A,HCLS1,ARHGDIB,COL6A3,COL1A1,COL6A1,DST,217718_s_at,RPL36A,NARS,COL4A2,COL4A1,CDKN1B,NDUFA13,PCM1,ZNF638,NACA,EXOSC4,COL3A1,COL1A2,ARPC5,EIF5,NPM1,MBNL1,PAPOLA,SH3BGRL,TPT1,PRDX2,COPS2,COPE,XPO1,DDX3X,EEF1A1,HNRPA1\nribosome\tRPL37,RPL37,RPS11,RPS21,RPL21,RPL36,RPL12,RPLP2,RPS19,RPL27,RPL31,LOC440055,LOC388344,RPL27A,RPL28,LOC283412,RPL19,RPS16,RPL36A,RPS6,RPS2,U16,RPL18A,RPL13,RPL29,RPS3A,RPS18,RPL14,RPL22,RPL34,RPS23,RPL15,RPL9,RPL6,RPL4,RPS4X,RPS24,RPL5,RPL23,MDS1,RPS7\nintracellular\tRPL37,RPL37,RAP1B,MGC2474,RPS21,RPL21,RPLP2,G3BP,GMFG,RPS19,RAB6C,PRKD2,RPL31,ZNF160,TGFB1I1,MEFV,GLTSCR2,MRPS12,CD74,RPL28,LOC283412,RPL19,ROCK1,SON,RPS6,RPS2,U16,RPL18A,RPL29,RPS18,RPL14,RPL22,RPL34,RPL15,RPL4,RPS4X,RPS24,RPL13A,RPL5,MDS1,RPS7\nintegral to plasma membrane\tHNRPM,HNRPM,IL2RG,HLA-DQA1,CD48,CD37,CLPTM1,ADRM1,HLA-B,CD164,HLA-DRA,SELL,HLA-DRB5,SLC25A11,HLA-DRB1,SLC25A3,HLA-DRB4,LAPTM5,CD2,C6orf12,IFNGR1,HLA-DPA1,ATP2A3,IGHA1,HLA-A,CAV1,SPN,IGHG3,CD52,IL6ST,HLA-C,IGHM,ATP8B1\ncytosolic small ribosomal subunit (sensu Eu...\tRPS15,RPS15,RPS5,FAU,RPS11,RPS21,RPS13,RPS19,LOC440055,RPS27,RPS10,RPS17,RPS16,RPS14,RPS6,RPS2,RPS15A,RPS20,RPS3A,RPS18,RPS27A,RPS23,RPS24,RPS9,RPS7,LOC402057,RPS25\ncytosolic large ribosomal subunit (sensu Eu...\tRPL35,RPL35,RPL18,RPL21,RPL36,RPLP2,RPL27A,RPL28,LOC283412,RPL7,U16,RPL18A,RPL29,RPL14,RPL22,RPL34,RPL6,RPL4,RPL5,MDS1\nplasma membrane\tARF1,ARF1,CD48,PTPRCAP,CD37,IL10RA,MSN,CD164,HLA-DRA,SELL,PRKAR2A,CD53,TRB@,TRBV3-1,TRA@,LCK,CD7,IL6ST\nmitochondrion\tACO2,ACO2,ATPIF1,MSRB2,SDHA,UCP2,TUFM,LOC440055,GPX4,SLC25A3,PPP2CA,NDUFA13,FIBP,DBT,NDUFB4,CYC1,RPA1\ncytosol\tCAST,CAST,NCF1,NPEPPS,KNS2,PSMB9,PSMB8,HSP90AA1,OGT,PPP2CA,HSP90B1,EIF5,RPS3A,EIF3S3,SPG21,COPB\nmembrane fraction\tRGS19,RGS19,ADRM1,HLA-B,PRKAR2A,LMOD1,COL6A3,IGHV1-69,IGHA1,U16,CD7,IGHG3,CD52,FIBP,RPL4,IGHM\nendoplasmic reticulum\tCAST,CAST,P4HB,SSR1,TAPBP,RPN1,PPIB,TLOC1,NAPA,HSP90B1,CAV1,KTN1,SET,TRAM1\ncytoskeleton\tCALD1,CALD1,RAVER2,ACTA2,TPM1,MSN,NPEPPS,ARHGDIB,LMOD1,ACTG2,ARPC3,ARPC5\nribonucleoprotein complex\tHNRPM,HNRPM,SRP72,HNRPH3,HNRPU,HNRPK,HNRPR,HNRPA3P1,SIAHBP1,HNRPA1\nextracellular region\tCAST,CAST,P4HB,CNOT2,MASP1,IGL@,FBLN1,IFI30,COL4A1,MIF,IGHG3,IGKC\nMHC class I protein complex\tHLA-J,HLA-J,RPLP2,HLA-B,HLA-E,C6orf12,HLA-F,HLA-A,HLA-G,HLA-C\nheterogeneous nuclear ribonucleoprotein com...\tHNRPH3,HNRPH3,HNRPU,HNRPH1,HNRPR,HNRPDL,HNRPA2B1,HNRPA1\nmitochondrial inner membrane\tNDUFA7,NDUFA7,SLC25A16,UCP2,SLC25A11,SLC25A3,NDUFV1\nextracellular space\tCNOT2,CNOT2,IL16,AD7C-NTP,FBLN1,TGFB1,MMP2,SPN,TPT1\nnucleoplasm\tIFI16,IFI16,NDUFA13,ZNF638,XPO1,ISG20,RNPC2,HNRPA1\ncollagen\tCOL1A1,COL1A1,COL6A1,COL4A2,COL4A1,COL3A1,COL1A2\nnucleolus\tRPL35,RPL35,RPS19,IFI16,NCL,EXOSC4,NPM1,RPS7\nspliceosome complex\tHNRPM,HNRPM,HIATL1,SF3B1,HNRPA2B1,HNRPA1\nproton-transporting two-sector ATPase complex\tATP6V0B,ATP6V0B,ATP6V0C,ATP5D,ATP5G3\nextracellular matrix (sensu Metazoa)\tLAMC1,LAMC1,FBLN1,DGCR6,COL6A3,TGFB1\nperinuclear region\t217718_s_at,217718_s_at,HSP90B1,SET\nDNA-directed RNA polymerase II, core complex\tPOLR2J,POLR2J,HCLS1,HMHA1,POLR2E\nactin cytoskeleton\tMARCKS,MARCKS,FNBP4,CORO1A,SMTN\ncAMP-dependent protein kinase complex\tPRKAR1A,PRKAR1A,PRKAR2A,PRKACB\nArp2/3 protein complex\tACTR3,ACTR3,ARPC1B,ARPC3,ARPC5\nlysosome\tHLA-DRA,HLA-DRA,LAPTM5,IFI30\nlarge ribosomal subunit\tRPL7,RPL7,RPL26,RPL13A,RPL17\nGolgi apparatus\tRGS19,RGS19,RAB6A,RAB6C,NAPA\nsoluble fraction\tNCF1,NCF1,FBLN1,NARS,PPP2CA\neukaryotic translation initiation factor 4F...\tEIF4G1,EIF4G1,EIF4A2,EIF4G2\nproton-transporting ATP synthase complex (s...\tATP5J2,ATP5J2,ATP5D,ATP5G3\nmicrosome\tSPCS2,SPCS2,TAPBP,HSP90B1\ncytosolic ribosome (sensu Eukaryota)\tLOC388344,LOC388344,RPL13\nGolgi membrane\tTAPBP,TAPBP,VDP,CAV1,COPB\ntranscription factor complex\tLOC342346,LOC342346,SUB1\nendoplasmic reticulum membrane\tTAPBP,TAPBP,HSP90B1,KTN1\nactin filament\tACTA2,ACTA2,ACTG2,IQGAP1\nproteasome complex (sensu Eukaryota)\tPSMB3,PSMB3,PSMB9,PSMB8\nnuclear envelope\tPAFAH1B1,PAFAH1B1,XPO1\nendoplasmic reticulum lumen\tCAST,CAST,PPIB,HSP90B1\nstress fiber\tPLEKHC1,PLEKHC1,SEPT7\nsmall ribosomal subunit\tRPS5,RPS5,MRPS12,RPS2\nmitochondrial electron transport chain\tUQCRC1,UQCRC1,NDUFA13\nmitochondrial ribosome\tMRPS12,MRPS12,MRPL34\nmitochondrial inner membrane presequence tr...\tTIMM8B,TIMM8B,TIMM13\nmediator complex\tTHRAP5,THRAP5,THRAP1\ncollagen type VI\tCOL6A3,COL6A3,COL6A1\nER-Golgi intermediate compartment\tP4HB,P4HB,TMED5,KTN1\nrespiratory chain complex III (sensu Eukary...\tUQCRQ,UQCRQ,UQCRFS1\neukaryotic translation elongation factor 1 ...\tEEF1B2,EEF1B2,EEF1D\ncytoplasmic membrane-bound vesicle\tARHGDIB,ARHGDIB,DST\nmicrotubule associated complex\tMEFV,MEFV,PAFAH1B1\nubiquitin ligase complex\tRNF11,RNF11,UBE4A\nprotein complex\tPSMB9,PSMB9,PSMB8\nproteasome core complex (sensu Eukaryota)\tPSMB9,PSMB9,PSMB8\nnuclear pore\tKPNB1,KPNB1,RGPD5\nintegrin complex\tITGAV,ITGAV,ITGB2\nsignal recognition particle (sensu Eukaryota)\tSRP72,SRP72,SRP9\ncell surface\tP4HB,P4HB,ATPIF1\nmyosin\tMYH9,MYH9,MYH11\nmitochondrial envelope\tUCRC,UCRC,COX5B\ncentrosome\tNPM1,NPM1,CEP27\nbasement membrane\tLAMC1,LAMC1,DST\nmitochondrial matrix\tMDH2,MDH2,ETFB\nCOPI vesicle coat\tCOPE,COPE,COPB\npericentriolar material\tPCM1,PCM1,LCK\nintermediate filament\tDES,DES,RPL38\nlipid raft\tLCK,LCK,CAV1\n',
      },
      {
        kind: 'colors',
        name: 'Co-expression MCODE clusters',
        fileName: 'NORMA_Human_coexpression_Expression_MCODE.txt',
        text: '207730_x_at\tgreen\n214836_x_at\tgreen\n215176_x_at\tgreen\n215182_x_at\tgreen\n216342_x_at\tgreen\n216524_x_at\tgreen\n217281_x_at\tgreen\n220725_x_at\tgreen\n221651_x_at\tgreen\nACTA2\tgreen\nACTR2\tgreen\nACTR3\tgreen\nAD7C-NTP\tgreen\nAP2S1\tgreen\nARF5\tgreen\nARHGDIB\tgreen\nARL6IP2\tgreen\nARPC3\tgreen\nATP8B1\tgreen\nATRX\tgreen\nAURKAIP1\tgreen\nBIRC2\tgreen\nC6orf12\tgreen\nCAV1\tgreen\nCD52\tgreen\nCD74\tgreen\nCDC5L\tgreen\nCEP27\tgreen\nCHCHD2\tgreen\nCOL1A1\tgreen\nCOL1A2\tgreen\nCOL3A1\tgreen\nCOL6A1\tgreen\nCOPE\tgreen\nCOX5A\tgreen\nCOX5B\tgreen\nCOX8A\tgreen\nCROP\tgreen\nCTA-246H3.1\tgreen\nCYBA\tgreen\nDBT\tgreen\nDDT\tgreen\nDDX3X\tgreen\nDDX5\tgreen\nEIF3S3\tgreen\nEIF3S6\tgreen\nEIF4G2\tgreen\nFAU\tgreen\nFBLN1\tgreen\nFBXW12\tgreen\nFLJ11021\tgreen\nFLJ12151\tgreen\nFLJ20294\tgreen\nFLJ42393\tgreen\nGNB2L1\tgreen\nH3F3A\tgreen\nHIATL1\tgreen\nHLA-A\tgreen\nHLA-B\tgreen\nHLA-C\tgreen\nHLA-DMA\tgreen\nHLA-DPA1\tgreen\nHLA-DQB1\tgreen\nHLA-DRB1\tgreen\nHLA-DRB4\tgreen\nHLA-DRB5\tgreen\nHLA-E\tgreen\nHLA-F\tgreen\nHLA-G\tgreen\nHLA-J\tgreen\nHNRPDL\tgreen\nHNRPH3\tgreen\nIFI30\tgreen\nIGHA1\tgreen\nIGHG3\tgreen\nIGHM\tgreen\nIGHV1-69\tgreen\nIGKC\tgreen\nIGKV1-5\tgreen\nIGKV1D-13\tgreen\nIGKV1OR15-118\tgreen\nIGKV1OR2-108\tgreen\nIGL@\tgreen\nIGLC1\tgreen\nIGLC2\tgreen\nIGLJ3\tgreen\nIGLV3-25\tgreen\nIQGAP1\tgreen\nKNS2\tgreen\nLAPTM5\tgreen\nLCK\tgreen\nLMOD1\tgreen\nLOC152719\tgreen\nLOC388344\tgreen\nLOC440055\tgreen\nLOC91316\tgreen\nMAN2B1\tgreen\nMATR3\tgreen\nMBNL1\tgreen\nMCM3AP\tgreen\nMDS1\tgreen\nMGEA5\tgreen\nMORF4L1\tgreen\nMRPL34\tgreen\nMT1E\tgreen\nMT1F\tgreen\nMT1G\tgreen\nMT1H\tgreen\nMT1L\tgreen\nMT1M\tgreen\nMT1X\tgreen\nMT2A\tgreen\nMYH11\tgreen\nNACA\tgreen\nNAP1L1\tgreen\nNARS\tgreen\nNDUFA2\tgreen\nNDUFA3\tgreen\nNDUFAB1\tgreen\nNDUFB11\tgreen\nNDUFB2\tgreen\nNDUFB4\tgreen\nNDUFS3\tgreen\nNDUFS7\tgreen\nNPM1\tgreen\nPAPOLA\tgreen\nPCNP\tgreen\nPDE4C\tgreen\nPFAAP5\tgreen\nPGF\tgreen\nPNRC2\tgreen\nPOLR2J\tgreen\nPRG1\tgreen\nPRR11\tgreen\nPTGES3\tgreen\nRAC2\tgreen\nRIOK3\tgreen\nROCK1\tgreen\nRPL11\tgreen\nRPL13\tgreen\nRPL13A\tgreen\nRPL17\tgreen\nRPL18\tgreen\nRPL21\tgreen\nRPL22\tgreen\nRPL23\tgreen\nRPL24\tgreen\nRPL27\tgreen\nRPL27A\tgreen\nRPL28\tgreen\nRPL29\tgreen\nRPL30\tgreen\nRPL34\tgreen\nRPL35A\tgreen\nRPL36\tgreen\nRPL36A\tgreen\nRPL38\tgreen\nRPL4\tgreen\nRPL5\tgreen\nRPL6\tgreen\nRPL7\tgreen\nRPL9\tgreen\nRPLP2\tgreen\nRPS14\tgreen\nRPS17\tgreen\nRPS18\tgreen\nRPS19\tgreen\nRPS2\tgreen\nRPS23\tgreen\nRPS24\tgreen\nRPS25\tgreen\nRPS27A\tgreen\nRPS3A\tgreen\nRPS4X\tgreen\nRPS5\tgreen\nRPS6\tgreen\nRPS7\tgreen\nRPS9\tgreen\nSEPT2\tgreen\nSERBP1\tgreen\nSFRS11\tgreen\nSH3BGRL\tgreen\nSKP1A\tgreen\nSLC35E1\tgreen\nSON\tgreen\nSUMO2\tgreen\nTAF7\tgreen\nTLOC1\tgreen\nTRA@\tgreen\nTRB@\tgreen\nTUG1\tgreen\nU16\tgreen\nUQCR\tgreen\nUQCRC1\tgreen\nUQCRFS1\tgreen\nUSP34\tgreen\nXPO1\tgreen\nYTHDC1\tgreen\nZNF160\tgreen\nZNF611\tgreen\nZNF638\tgreen\nZNHIT1\tgreen\n208246_x_at\tyellow\n211637_x_at\tyellow\n211645_x_at\tyellow\nATP5G3\tyellow\nATP6V0B\tyellow\nATP6V0D1\tyellow\nBCLAF1\tyellow\nCAPZA1\tyellow\nCOL6A3\tyellow\nCXCR4\tyellow\nHLA-DPB1\tyellow\nHNRPH1\tyellow\nIFI16\tyellow\nLOC342346\tyellow\nLOC56902\tyellow\nLOC58486\tyellow\nLOC645745\tyellow\nMEFV\tyellow\nMRPS12\tyellow\nNDUFS8\tyellow\nPTRF\tyellow\nRPL31\tyellow\nRPL35\tyellow\nRPS11\tyellow\nTAGLN\tyellow\nTGFB1\tyellow\nTRBC1\tyellow\n208120_x_at\tred\n208238_x_at\tred\n211639_x_at\tred\n211641_x_at\tred\n211650_x_at\tred\n211908_x_at\tred\n212498_at\tred\n216412_x_at\tred\n216858_x_at\tred\n217052_x_at\tred\n217258_x_at\tred\n217679_x_at\tred\n217718_s_at\tred\n218041_x_at\tred\nAASDHPPT\tred\nACO2\tred\nACTG2\tred\nACTR10\tred\nADD3\tred\nADRM1\tred\nAKR7A2\tred\nAKT1\tred\nALOX5AP\tred\nARF1\tred\nARL6IP5\tred\nARL8B\tred\nARPC1B\tred\nARPC5\tred\nARPP-19\tred\nATP2A3\tred\nATP5D\tred\nATP5J2\tred\nATP6AP2\tred\nATP6V0C\tred\nATPIF1\tred\nBTF3\tred\nC11orf2\tred\nC11orf58\tred\nC12orf10\tred\nC12orf38\tred\nC16orf24\tred\nC17orf62\tred\nC1orf160\tred\nC1orf63\tred\nC21orf33\tred\nC3orf60\tred\nCAB39\tred\nCALD1\tred\nCAP1\tred\nCAPNS1\tred\nCAST\tred\nCBX3\tred\nCCL5\tred\nCCNL1\tred\nCD164\tred\nCD2\tred\nCD37\tred\nCD3D\tred\nCD48\tred\nCD53\tred\nCD7\tred\nCD79A\tred\nCDKN1B\tred\nCECR1\tred\nCHMP2A\tred\nCLPP\tred\nCLPTM1\tred\nCLTC\tred\nCNN1\tred\nCNOT2\tred\nCOL4A1\tred\nCOL4A2\tred\nCOPB\tred\nCOPS2\tred\nCOQ9\tred\nCORO1A\tred\nCOX4I1\tred\nCSDE1\tred\nCSNK1A1\tred\nCUL4B\tred\nCXorf9\tred\nCYC1\tred\nDDX1\tred\nDDX17\tred\nDDX42\tred\nDDX50\tred\nDES\tred\nDEXI\tred\nDGCR6\tred\nDHX15\tred\nDIP2A\tred\nDKFZP566N034\tred\nDST\tred\nDYNLT3\tred\nECH1\tred\nEEF1A1\tred\nEEF1B2\tred\nEEF1D\tred\nEIF4A2\tred\nEIF4G1\tred\nEIF5\tred\nENDOG\tred\nERH\tred\nETFB\tred\nEXOSC4\tred\nFAIM3\tred\nFAM96B\tred\nFIBP\tred\nFIS1\tred\nFLJ10154\tred\nFLJ14346\tred\nFNBP4\tred\nFNTA\tred\nFXYD5\tred\nG3BP\tred\nGBL\tred\nGLTSCR2\tred\nGMFG\tred\nGOLGA8A\tred\nGOLGA8B\tred\nGPAA1\tred\nGPSM3\tred\nGPX1\tred\nGPX4\tred\nGTF2I\tred\nGUK1\tred\nHCLS1\tred\nHLA-DMB\tred\nHLA-DQA1\tred\nHLA-DRA\tred\nHMGB1\tred\nHMHA1\tred\nHNRPA1\tred\nHNRPA2B1\tred\nHNRPA3P1\tred\nHNRPK\tred\nHNRPM\tred\nHNRPR\tred\nHNRPU\tred\nHSP90AA1\tred\nHSP90B1\tred\nIFNGR1\tred\nIGHD\tred\nIGLV3-10\tred\nIL10RA\tred\nIL16\tred\nIL2RG\tred\nIL6ST\tred\nIL7R\tred\nINPP5D\tred\nIRF7\tred\nISG20\tred\nITGAV\tred\nITGB2\tred\nKIAA0907\tred\nKIAA1840\tred\nKIDINS220\tred\nKPNB1\tred\nKTN1\tred\nLAMC1\tred\nLOC283412\tred\nLOC402057\tred\nLRRFIP1\tred\nLTB\tred\nLYSMD4\tred\nMARCKS\tred\nMASP1\tred\nMDH2\tred\nMGC2474\tred\nMIF\tred\nMMP2\tred\nMRP63\tred\nMSN\tred\nMSRB2\tred\nMYH9\tred\nMYLK\tred\nNAPA\tred\nNBPF1\tred\nNBPF12\tred\nNCF1\tred\nNCL\tred\nNDUFA10\tred\nNDUFA13\tred\nNDUFA7\tred\nNDUFB6\tred\nNDUFC1\tred\nNDUFV1\tred\nNFE2L2\tred\nNPEPPS\tred\nNPTN\tred\nOAZ1\tred\nOAZ2\tred\nOGT\tred\nOPHN1\tred\nORC6L\tred\nOSBPL8\tred\nP4HB\tred\nPAFAH1B1\tred\nPCM1\tred\nPDCD10\tred\nPGLS\tred\nPHIP\tred\nPLEKHC1\tred\nPOLR1B\tred\nPOLR2E\tred\nPOLR2I\tred\nPOLR2L\tred\nPOPDC3\tred\nPPIB\tred\nPPP1CA\tred\nPPP2CA\tred\nPRDX2\tred\nPRKACB\tred\nPRKAR1A\tred\nPRKAR2A\tred\nPRKD2\tred\nPSMB3\tred\nPSMB8\tred\nPSMB9\tred\nPTPRCAP\tred\nPUM2\tred\nR3HCC1\tred\nRAB1A\tred\nRAB6A\tred\nRAB6C\tred\nRAP1B\tred\nRAVER2\tred\nRBPMS\tred\nREEP5\tred\nRGPD5\tred\nRGS19\tred\nRNF11\tred\nRNPC2\tred\nRPA1\tred\nRPL10A\tred\nRPL12\tred\nRPL14\tred\nRPL15\tred\nRPL18A\tred\nRPL19\tred\nRPL23A\tred\nRPL26\tred\nRPL32\tred\nRPL37\tred\nRPL39\tred\nRPL41\tred\nRPN1\tred\nRPS10\tred\nRPS13\tred\nRPS15\tred\nRPS15A\tred\nRPS16\tred\nRPS20\tred\nRPS21\tred\nRPS27\tred\nSCAND1\tred\nSDHA\tred\nSEC61A1\tred\nSELL\tred\nSEPT7\tred\nSET\tred\nSF3B1\tred\nSFPQ\tred\nSFRS10\tred\nSFRS3\tred\nSFRS5\tred\nSIAHBP1\tred\nSLC25A11\tred\nSLC25A16\tred\nSLC25A3\tred\nSLC38A2\tred\nSMTN\tred\nSPARCL1\tred\nSPCS2\tred\nSPG21\tred\nSPN\tred\nSRP46\tred\nSRP72\tred\nSRP9\tred\nSSR1\tred\nSTAT1\tred\nSUB1\tred\nSUMO1\tred\nTAPBP\tred\nTARDBP\tred\nTCEA1\tred\nTCEB2\tred\nTGFB1I1\tred\nTHRAP1\tred\nTHRAP5\tred\nTIMM13\tred\nTIMM8B\tred\nTINP1\tred\nTMED5\tred\nTMEM123\tred\nTMEM66\tred\nTMEM93\tred\nTOMM20\tred\nTPM1\tred\nTPT1\tred\nTRAM1\tred\nTRBV3-1\tred\nTRIM22\tred\nTTC19\tred\nTTC3\tred\nTUFM\tred\nTXNL2\tred\nUBE2D2\tred\nUBE4A\tred\nUCP2\tred\nUCRC\tred\nUQCRQ\tred\nUSP47\tred\nVDP\tred\nWAC\tred\nWDR45\tred\nWIPI2\tred\nWSB1\tred\nYIPF3\tred\nYME1L1\tred\nYWHAZ\tred\nZC3H11A\tred\nZFR\tred\nZMYND11\tred\nZNF403\tred\nZNF552\tred\nZNF9\tred\nhfl-B5\tred\n',
      },
    ],
  },
  covid: {
    title: 'COVID-19 (IntAct)',
    source: 'IntAct database',
    link: null,
    files: [
      {
        kind: 'network',
        name: 'COVID-19 IntAct',
        fileName: 'Intact-data_COVID19_no_self_loops.txt',
        text: 'Source\tTarget\nP31809\tP11224\nP33767\tP41811\nQ3T133\tP41811\nP39656\tP41811\nP41811\tO15503\nQ91AV1\tP41811\nQ96WV5\tP68978\nQ9Y5U4\tP41811\nP53622\tP41811\nP53622\tQ3T133\nP53622\tP41811\nP53622\tP33767\nP53622\tP41811\nP53622\tP68978\nP53622\tP41811\nP53622\tP39656\nQ3T133\tQ96WV5\nQ96WV5\tP39656\nQ96WV5\tP33767\nQ96WV5\tP68978\nP53622\tP41811\nP53622\tQ9Y5U4\nP53622\tP41811\nP53622\tQ91AV1\nP53622\tP41811\nP53622\tO15503\nQ96WV5\tQ9Y5U4\nQ96WV5\tO15503\nQ91AV1\tQ96WV5\nP53622\tP41811\nP53622\tP41811\nP53622\tP41811\nP53622\tP41811\nP53622\tP41811\nP27487\tK0BRG7\nP27487\tK0BRG7\nQ9BYF1-1\tP59594\nP27487\tK0BRG7\nQ9BYF1-1\tP59594\nP27487\tK0BRG7\nP27487\tK0BRG7\nP0C6U8\tP62753\nQ9BYF1-1\tQ6Q1S2\nQ9BYF1-1\tQ6Q1S2\nQ9BYF1-1\tQ6Q1S2\nQ7TLC7\tO14964\nQ7TLC7\tP05155\nQ7TLC7\tQ13561\nQ7TLC7\tQ9NRH1\nJ9TC74\tQ13561\nJ9TC74\tP62258\nJ9TC74\tQ99471\nP59633\tQ13561\nP59633\tP05155\nP59633\tP02768\nQ6S8E0\tP08708\nQ6S8E0\tP25787\nP59634\tQ13561\nP59634\tQ92994\nQ19QW4\tQ9BQB6\nQ19QW4\tQ9H4F8\nQ7T6S2\tP49069\nQ7T6S2\tP05155\nQ6S8E0\tP60866\nQ6S8E0\tP27448\nQ19QW2\tP46379\nQ6S8E0\tP60866\nQ19QW2\tP46379\nP59634\tQ13561\nQ7T6S2\tP49069\nQ19QW4\tQ9BQB6\nQ6S8E0\tP27448\nQ6S8E0\tP25787\nQ7TLC7\tO14964\nQ7TLC7\tQ13561\nQ1HVL3\tP15144\nQ1HVL8\tP15144\nQ1HVK9\tP15144\nQ9BYF1\tP0DTC2\nQ9BYF1\tP59594\nK9N5Q8\tP27487\nQ9BYF1\tP0DTC2\nQ9BYF1\tP59594\nQ9BYF1\tP0DTC2\nQ9BYF1\tP59594\nQ9BYF1\tP0DTC2\nQ9BYF1\tP59594\nQ9BYF1\tP59594\nP61769\tP04439\nP61769\tP59595\nP61769\tP04439\nP61769\tP03407\nP61769\tP04439\nP61769\tP59595\nP61769\tP04439\nP61769\tP59596\nP61769\tP04439\nP61769\tQ692E0\nP61769\tP04439\nP61769\tP59596\nP61769\tP04439\nP61769\tQ76R37\nP61769\tP04439\nP61769\tP59637\nQ9BYF1\tP0DTC2\nQ9BYF1\tP0DTC2\nQ9BYF1\tP0DTC2\nQ9BYF1\tP0DTC2\nQ9BYF1\tP59594\nQ9BYF1\tP0DTC2\nQ9BYF1\tP0DTC2\nQ9BYF1\tQ695T7\nQ9BYF1\tQ695T7\nQ9BYF1\tP0DTC2\nQ9BYF1\tQ695T7\nQ9BYF1\tP37173\nQ9BYF1\tP37173\nP11021\tK0BRG7\nP11021\tK0BRG7\nP11021\tK0BRG7\nP11021\tK0BRG7\nP11021\tK0BRG7\nP11021\tK0BRG7\nP11021\tK0BRG7\nP11021\tK0BRG7\nK0BRG7\tP11021\nP0DTC4\tQ8IWA5\nP0DTC4\tQ86VM9\nP0DTC4\tQ6UX04\nP0DTC4\tP25440\nP0DTC4\tO60885\nP0DTC4\tO00203\nP0DTC9\tQ9Y3U8\nP0DTC9\tQ9NW13\nP0DTC9\tQ9NR30\nP0DTC9\tQ9UN86\nP0DTC9\tQ9HCE1\nP0DTC9\tQ92900\nP0DTC9\tQ8TAD8\nP0DTC9\tQ8NCA5\nP0DTC9\tQ6PKG0\nP0DTC9\tQ13310\nP0DTC9\tQ13283\nP0DTC9\tP67870\nP0DTC9\tP19784\nP0DTC9\tP11940\nP0DTC9\tO43818\nP0DTC2\tQ9C0B5\nP0DTC2\tQ7Z5G4\nP0DTC5\tQ9Y6E2\nP0DTC5\tQ9Y312\nP0DTC5\tQ9ULX6\nP0DTC5\tQ9UDR5\nP0DTC5\tQ9UBU6\nP0DTC5\tQ9NQC3\nP0DTC5\tQ9BW92\nP0DTC5\tQ9BSJ2\nP0DTC5\tQ9BQT8\nP0DTC5\tQ96HW7\nP0DTC5\tQ96HR9\nP0DTC5\tQ96ER3\nP0DTC5\tQ96D53\nP0DTC5\tQ96CW5\nP0DTC5\tQ8NEW0\nP0DTC5\tQ7L8L6\nP0DTC5\tQ6PML9\nP0DTC5\tQ5JRX3\nP0DTC5\tQ4KMQ2\nP0DTC5\tQ10713\nP0DTC5\tQ00765\nP0DTC5\tP48556\nP0DTC5\tP38606\nP0DTC5\tP38435\nP0DTC5\tP27105\nP0DTC5\tP13804\nP0DTC5\tP11310\nP0DTC5\tP05026\nP0DTC5\tO95070\nP0DTC5\tO75439\nP0DTC6\tQ9NZJ7\nP0DTC6\tP78406\nP0DTC6\tP52948\nP0DTC3\tQ9Y673\nP0DTC3\tQ9UH99\nP0DTC3\tQ9H270\nP0DTC3\tQ96S66\nP0DTC3\tQ96JC1\nP0DTC3\tQ8N6S5\nP0DTC3\tQ8IWR1\nP0DTC3\tP09601\nP0DTC7\tQ9NU22\nP0DTC7\tQ7Z4Q2\nP0DTC8\tQ96IV0\nP0DTC8\tQ8N0Z8\nP0DTC8\tQ96AY3\nP0DTC8\tQ13443\nP0DTC8\tQ9Y680\nP0DTC8\tQ9Y4L1\nP0DTC8\tQ9UHI8\nP0DTC8\tQ9P2E5\nP0DTC8\tQ9NYU1\nP0DTC8\tQ9NXK8\nP0DTC8\tQ9H8W4\nP0DTC8\tQ9H4F8\nP0DTC8\tQ9H488\nP0DTC8\tQ9H173\nP0DTC8\tQ9BZQ6\nP0DTC8\tQ9BS26\nP0DTC8\tQ9BRN9\nP0DTC8\tQ99988\nP0DTC8\tQ99519\nP0DTC8\tQ99470\nP0DTC8\tQ96MM7\nP0DTC8\tQ96F46\nP0DTC8\tQ96DZ1\nP0DTC8\tQ92820\nP0DTC8\tQ8N766\nP0DTC8\tQ8IZ52\nP0DTC8\tQ8IWF2\nP0DTC8\tQ8IV08\nP0DTC8\tQ86YB8\nP0DTC8\tQ7Z4H8\nP0DTC8\tQ6UW63\nP0DTC8\tQ15818\nP0DTC8\tQ13438\nP0DTC8\tQ08431\nP0DTC8\tP61916\nP0DTC8\tP58166\nP0DTC8\tP29122\nP0DTC8\tP28300\nP0DTC8\tP26358\nP0DTC8\tP15151\nP0DTC8\tP12109\nP0DTC8\tP0C7P0\nP0DTC8\tP05556\nP0DTC8\tP00750\nP0DTC8\tO76061\nP0DTC8\tO14656\nP0DTC8\tO00469\nP0DTD2\tQ9UL15\nP0DTD2\tQ9UKA9\nP0DTD2\tQ9P0L2\nP0DTD2\tQ9H773\nP0DTD2\tQ9H2P9\nP0DTD2\tQ7KZI7\nP0DTD2\tP27448\nP0DTD2\tO94826\nP0DTD2\tO75534\nP0DTD2\tO43633\nP0DTD2\tO14745\nP0DTD3\tQ9Y3A6\nP0DTD3\tQ2PZI1\nP0DTD3\tQ96S52\nP0DTD3\tQ9Y6M9\nP0DTD3\tQ9Y375\nP0DTD3\tQ9Y276\nP0DTD3\tQ9NV92\nP0DTD3\tQ9H845\nP0DTD3\tQ9H3K2\nP0DTD3\tQ9GZU3\nP0DTD3\tQ9BVK2\nP0DTD3\tQ9BQ95\nP0DTD3\tQ96K12\nP0DTD3\tQ8TEQ8\nP0DTD3\tQ86VR2\nP0DTD3\tQ86UT6\nP0DTD3\tQ7Z2K6\nP0DTD3\tQ6NXT6\nP0DTD3\tQ6NXT4\nP0DTD3\tQ5BJF2\nP0DTD3\tQ12770\nP0DTD3\tP55085\nP0DTD3\tP33527\nP0DTD3\tO76024\nP0DTD3\tO43292\nP0DTD3\tO00124\nQ1LZX8\tP59594\nQ1LZX8\tP84198\nQ1LZX8\tP84198\nQ1LZX8\tP59594\nP84198\tP59594\nP84198\tP59594\nP59637\tQ64373\nP59637\tO35843\nQ64373\tQ07817\nQ64373\tP59637\nP59637\tO35843\nP59637\tQ07817\nP06731-1\tK0BRG7\nP06731\tK0BRG7\nK0BRG7\tP06731\nP06731\tK0BRG7\nP06731\tK0BRG7\nP59637\tP59636\nP59633\tP59637\nQ7TFA0\tP59636\nQ7TFA1\tQ19QW5\nQ80H93\tQ7TFA0\nP59596\tP59632\nP59594\tP59635\nQ7TFA1\tP59637\nP59594\tQ80H93\nQ7TFA1\tQ80H93\nQ7TFA0\tQ80H93\nQ7TLC7\tP59636\nQ7TFA1\tP59636\nQ80H93\tP59636\nQ7TLC7\tQ80H93\nQ7TFA1\tP59637\nP59595\tP59596\nQ7TFA1\tQ19QW5\nQ7TLC7\tP59636\nQ7TLC7\tP59636\nQ9BYF1\tP02768\nF1RG45\tK7GLM4\nQ9BYF1\tP05814\nQ9BYF1\tP0DTC2\nQ9BYF1\tP59594\nQ92793\tQ14653\nQ92793\tQ14653\nQ07817\tP59635\nP59635\tP10415\nQ07820\tP59635\nQ16548\tP59635\nP59635\tQ92843\nQ9BYF1\tP59594\nQ9BYF1\tQ6Q1S2\nQ9BYF1\tP59594\nQ9BYF1\tQ6Q1S2\nQ9BYF1\tQ6Q1S2\nP59594\tP00747\nP59594\tP00762\nQ6ZMR5\tP59594\nQ9BYF1\tP59594\nQ9BYF1\tP59594\nQ9BYF1\tP59594\nQ9BYF1\tP59594\nQ9BYF1\tP59594\nQ9BYF1\tP59594\nP59594\tQ56NL1\nP59595\tP63279\nP59595\tP63279\nP59595\tP63279\nP59595\tP63279\nP20290\tQ86VG3\nQ86U32\tQ86VG3\nP24928\tQ86VG3\nP20290\tQ86VG3\nP20290\tQ86VG3\nP20290\tQ86VG3\nO00303\tP59594\nO00303\tP59594\nO00303\tP11223\nO00303\tP59594\nO00303\tP11223\nO00303\tP59594\nO00303\tP59594\nP59595\tQ05639\nP03070\tP02340\nP59595\tQ05639\nQ05639\tP59595\nP59595\tQ05639\nP59595\tQ05639\nP15130\tQ05639\nQ05639\tP59595\nQ05639\tP08107\nQ05639\tP59595\nQ05639\tP08107\nP59595\tQ05639\nP59595\tQ05639\nP59595\tQ05639\nP59595\tP63279\nP59595\tP63279\nP59595\tP63279\nP59595\tP63279\nP59595\tP63279\nP59595\tP63279\nP59595\tP63279\nP59595\tP63279\nP59595\tP62937\nP59595\tP62937\nP35613\tP62937\nQ9BYF1\tP59594\nP59594\tQ5EGZ1\nQ9BYF1\tP59594\nP59594\tQ5EGZ1\nP59594\tQ56NL1\nQ9BYF1\tQ5GDB5\nQ9BYF1\tP59594\nQ56NL1\tP59594\nQ9BYF1\tP59594\nP59594\tQ56NL1\nQ56NL1\tQ5GDB5\nQ9BYF1\tQ5GDB5\nQ9BYF1\tP59594\nP0DTC2\tP62269\nP0DTC2\tQ96N67\nP0DTC2\tP63244\nP0DTC2\tP40227\nP0DTC2\tP07195\nP0DTC2\tP08621\nP0DTC2\tP60660\nP0DTC2\tP47756\nP0DTC2\tP52907\nP0DTC2\tQ9NYL9\nP0DTC2\tP13797\nP0DTC2\tQ9ULV4\nP0DTC2\tP59998\nP0DTC2\tP61158\nP0DTC2\tQ9UM54\nP0DTC4\tQ9ULX6\nP0DTC4\tP50402\nP0DTC4\tQ00325\nP0DTC4\tP05023\nP0DTC4\tO75746\nP0DTC4\tP16615\nP0DTC4\tQ9P035\nP0DTC4\tQ5H9R7\nP0DTC5\tP30876\nP0DTC5\tO15397\nP0DTC5\tQ9H078\nP0DTC5\tP62195\nP0DTC5\tP26599\nP0DTC5\tO60762\nP0DTC5\tQ9H936\nP0DTC5\tO00231\nP0DTC5\tQ9ULX6\nP0DTC5\tQ9UHI6\nP0DTC5\tQ7Z4H7\nP0DTC5\tQ96EY1\nP0DTC5\tQ00325\nP0DTC5\tQ15008\nP0DTC5\tO14818\nP0DTC5\tP40939\nP0DTC5\tP50402\nP0DTC5\tQ5H9R7\nP0DTC5\tO95757\nP0DTC5\tP57678\nP0DTC5\tP31689\nP0DTC5\tO15027\nP0DTC5\tP60900\nP0DTC5\tP25705\nP0DTC5\tP51665\nP0DTC5\tP62191\nP0DTC5\tP06576\nP0DTC5\tQ14257\nP0DTC5\tQ99615\nP0DTC5\tO95816\nP0DTC5\tO95071\nP0DTC5\tP46379\nP0DTC5\tO75746\nP0DTC5\tQ9Y5A9\nP0DTC5\tQ13200\nP0DTC5\tP53618\nP0DTC5\tQ9NZ01\nP0DTC5\tP35998\nP0DTC5\tQ16891\nP0DTC5\tO60884\nP0DTC5\tO43852\nP0DTC5\tQ99460\nP0DTC5\tQ9NTJ3\nP0DTC5\tP53621\nP0DTC5\tP55786\nP0DTC5\tQ15293\nP0DTC5\tP34932\nP0DTC5\tQ9UBF2\nP0DTC5\tO43592\nP0DTC5\tP35606\nP0DTC5\tP05023\nP0DTC5\tO95347\nP0DTC5\tP16615\nP0DTC5\tO14980\nP0DTC5\tQ96CS3\nP0DTC5\tQ9P035\nP0DTC9\tP09622\nP0DTC9\tP08621\nP0DTC9\tP38159\nP0DTC9\tQ13151\nP0DTC9\tQ9UN86\nP0DTC9\tQ13283\nP0DTC9\tQ9UM54\nP0DTC3\tP40939\nP0DTC3\tQ14257\nP0DTC3\tP22314\nP0DTC3\tP04406\nP0DTC3\tP57088\nP0DTC3\tQ8IXB1\nP0DTC3\tQ00325\nP0DTC3\tP31689\nP0DTC3\tP25705\nP0DTC3\tO43852\nP0DTC3\tO95757\nP0DTC3\tP46379\nP0DTC3\tP53621\nP0DTC3\tQ9H936\nP0DTC3\tO60884\nP0DTC3\tP26599\nP0DTC3\tO75746\nP0DTC3\tP11586\nP0DTC3\tQ9NZ01\nP0DTC3\tP53007\nP0DTC3\tP05023\nP0DTC3\tQ96CS3\nP0DTC3\tO43592\nP0DTC3\tO95347\nP0DTC3\tP16615\nP0DTC3\tQ16891\nP0DTC3\tO14980\nP0DTC3\tQ9P035\nP0DTC6\tQ5SW79\nP0DTC6\tP25705\nP0DTC6\tQ92552\nP0DTC6\tP06576\nP0DTC6\tP15924\nP0DTC6\tP53396\nP0DTC6\tP16615\nP0DTC6\tQ5T9A4\nP0DTC6\tP05023\nP0DTC6\tO75592\nP0DTC6\tO75934\nP0DTC6\tO14980\nP0DTC6\tQ96CS3\nP0DTC6\tP78406\nP0DTC6\tQ9NVI7\nP0DTC6\tP33176\nP0DTC6\tQ13561\nP0DTC6\tQ6P2E9\nP0DTC7\tP30153\nP0DTC7\tP06576\nP0DTC7\tP40939\nP0DTC7\tQ8IXB1\nP0DTC7\tQ9UHI6\nP0DTC7\tO95816\nP0DTC7\tP53007\nP0DTC7\tQ96EY1\nP0DTC7\tP50402\nP0DTC7\tO43852\nP0DTC7\tQ9H936\nP0DTC7\tP62195\nP0DTC7\tP30876\nP0DTC7\tQ99615\nP0DTC7\tP55786\nP0DTC7\tP57678\nP0DTC7\tQ00325\nP0DTC7\tO75746\nP0DTC7\tO15027\nP0DTC7\tP34932\nP0DTC7\tQ15008\nP0DTC7\tQ9UNM6\nP0DTC7\tO95071\nP0DTC7\tO60884\nP0DTC7\tP62191\nP0DTC7\tP05023\nP0DTC7\tP51665\nP0DTC7\tP53618\nP0DTC7\tQ9NZ01\nP0DTC7\tP35998\nP0DTC7\tQ13200\nP0DTC7\tP53621\nP0DTC7\tQ5H9R7\nP0DTC7\tP16615\nP0DTC7\tQ99460\nP0DTC7\tO43592\nP0DTC7\tQ9NXS2\nP0DTC7\tP46379\nP0DTC7\tQ16891\nP0DTC7\tO95347\nP0DTC7\tP35606\nP0DTC7\tQ9UBF2\nP0DTC7\tQ9P035\nP0DTC7\tO14980\nP0DTC7\tQ96CS3\nP0DTD8\tQ14257\nP0DTD8\tO60762\nP0DTD8\tO43852\nP0DTD8\tP25705\nP0DTD8\tP31689\nP0DTD8\tP06576\nP0DTD8\tQ8WTT2\nP0DTD8\tQ7KZF4\nP0DTD8\tQ9NVI7\nP0DTD8\tP05023\nP0DTD8\tP55786\nP0DTD8\tQ00325\nP0DTD8\tP53618\nP0DTD8\tP57088\nP0DTD8\tO43592\nP0DTD8\tO95347\nP0DTD8\tQ9NZ01\nP0DTD8\tQ16891\nP0DTD8\tP16615\nP0DTD8\tP53621\nP0DTD8\tQ9NXS2\nP0DTD8\tQ96CS3\nP0DTD8\tQ02978\nP0DTD8\tP35606\nP0DTD8\tQ9UBF2\nP0DTD8\tQ9P035\nP0DTD8\tO14980\nP0DTD8\tP10155\nP0DTC8\tQ14152\nP0DTC8\tP50402\nP0DTC8\tQ9UHI6\nP0DTC8\tP06576\nP0DTC8\tP62195\nP0DTC8\tP25705\nP0DTC8\tP40939\nP0DTC8\tQ5JWF2\nP0DTC8\tO14818\nP0DTC8\tP55786\nP0DTC8\tP57678\nP0DTC8\tO43852\nP0DTC8\tQ12789\nP0DTC8\tQ9H936\nP0DTC8\tP31689\nP0DTC8\tQ15008\nP0DTC8\tQ9UNM6\nP0DTC8\tQ00325\nP0DTC8\tO15027\nP0DTC8\tP30876\nP0DTC8\tQ7KZF4\nP0DTC8\tQ9NVI7\nP0DTC8\tO60884\nP0DTC8\tQ13200\nP0DTC8\tP05023\nP0DTC8\tO95071\nP0DTC8\tP35998\nP0DTC8\tO43592\nP0DTC8\tQ02978\nP0DTC8\tQ9NZ01\nP0DTC8\tP53618\nP0DTC8\tQ99460\nP0DTC8\tO75746\nP0DTC8\tP16615\nP0DTC8\tP53621\nP0DTC8\tQ16891\nP0DTC8\tO95347\nP0DTC8\tO14980\nP0DTC8\tQ9P035\nP0DTC8\tQ9UBF2\nP0DTC8\tP46379\nP0DTD2\tP19105\nP0DTD2\tQ14315\nP0DTD2\tP62140\nP0DTD2\tP0DP23\nP0DTD2\tP09622\nP0DTD2\tP52907\nP0DTD2\tP60660\nP0DTD2\tQ9NYL9\nP0DTD2\tP13797\nP0DTD2\tQ9UM54\nP0DTD2\tQ9ULV4\nP0DTD2\tP59998\nP0DTD3\tQ9BUQ8\nP0DTD3\tQ96N67\nP0DTD3\tQ15365\nP0DTD3\tP17987\nP0DTD3\tO00231\nP0DTD3\tQ14257\nP0DTD3\tQ16576\nP0DTD3\tP62269\nP0DTD3\tQ58FF8\nP0DTD3\tP27348\nP0DTD3\tP40227\nP0DTD3\tP63244\nP0DTD3\tP49368\nP0DTD3\tQ9Y265\nP0DTD3\tP07355\nP0DTD3\tP46379\nP0DTD3\tP48643\nP0DTD3\tQ99832\nP0DTD3\tQ9UHI6\nP0DTD3\tP31948\nP0DTD3\tQ3ZCQ8\nP0DTD3\tQ9NVI7\nP0DTD3\tP46782\nP0DTD3\tP25205\nP0DTD3\tP30153\nP0DTD3\tQ9ULX6\nP0DTD3\tP42677\nP0DTD3\tP61158\nP0DTD3\tP09874\nP0DTD3\tP50402\nP0DTD3\tP30876\nP0DTD3\tQ9NXS2\nP0DTD3\tP40939\nP0DTD3\tP06493\nP0DTD3\tP51570\nP0DTD3\tQ7Z4H7\nP0DTD3\tP57088\nP0DTD3\tQ14739\nP0DTD3\tP10155\nP0DTD3\tQ9UM54\nP0DTD3\tP04406\nP0DTD3\tQ9H936\nP0DTD3\tP11586\nP0DTD3\tQ9UBX3\nP0DTD3\tQ5H9R7\nP0DTD3\tP53618\nP0DTD3\tP53007\nP0DTD3\tP12004\nP0DTD3\tP05023\nP0DTD3\tQ16891\nP0DTD3\tQ9NZ01\nP0DTD3\tO14980\nP0DTD3\tO95347\nP0DTD3\tQ9NTJ3\nP0DTD3\tP53621\nP0DTD3\tP16615\nP0DTD3\tO43592\nP0DTD3\tQ9UBF2\nP0DTD3\tP35606\nP0DTD3\tQ9P035\nP59594\tP35247\nP52294\tP52630\nP52294\tP42224\nP52292\tQ14974\nP52294\tP42224\nP52294\tQ14974\nP52292\tQ14974\nP52294\tP42224\nP52292\tQ14974\nP52292\tQ14974\nQ99623\tP35232\nQ99623\tP35232\nO35129\tP67778\nO35129\tQ9JIA7\nO35129\tP67778\nO35129\tP19783\nO35129\tP00397\nO35129\tP67778\nP00405\tP19783\nP00405\tP12787\nP00405\tP19536\nP00405\tQ9CPQ1\nP00405\tP56391\nP00405\tP56392\nP12904\tP06782\nP33469\tP13010\nP33469\tP39019\nP33469\tP61254\nP33469\tP16402\nP33469\tP09651\nP33469\tP67809\nP33469\tP0DMV8\nP33469\tP11940\nP33469\tP12956\nP33469\tQ00839\nP33469\tP19338\nP33469\tP67809\nP33469\tP12956\nP33469\tQ00839\nP33469\tP19338\nK0BWD0\tP05161\nP05161\tK0BWD0\nK0BWD0\tP05161\nK0BWD0\tP05161\nR9QB93\tK0BWD0\nO43765\tP59635\nO43765\tP59635\nP59635\tP59596\nP59635\tP59637\nQ5MAG3\tP59635\nO43765\tP59635\nP59595\tP59596\nP0DTC2\tP35613\nP0DTC2\tP35613\nP0DTC2\tP35613\nP0DTC2\tP35613\nP0DTC2\tP35613\nP04135\tF1S215\nP04135\tF1RNN5\nP04135\tA6M930\nP04135\tA6M930\nP04135\tA6M930\nP04135\tA6M930\nQ9BYF1\tP0DTC2\nQ9BYF1\tP59594\nQ9BYF1\tP0DTC2\nQ9BYF1\tP59594\nQ9BYF1\tP0DTC2\nQ9BYF1\tP59594\nQ9BYF1\tP0DTC2\nQ9BYF1\tP0DTC2\nQ53F19\tP56270\nQ53F19\tQ14011\nQ53F19\tQ8N0Z6\nQ53F19\tQ8N8U2\nQ53F19\tP35052\nQ53F19\tO95373\nQ53F19\tQ9UH99\nQ53F19\tQ8NEM0\nQ53F19\tQ32M45\nQ53F19\tQ6L8Q7\nQ53F19\tQ6PJT7\nQ53F19\tQ02224\nQ53F19\tP31150\nQ53F19\tQ5DJT8\nQ53F19\tQ9UER7\nQ53F19\tP48668\nQ53F19\tO75940\nQ53F19\tJ3QSV6\nQ53F19\tO95619\nQ53F19\tQ7L590\nQ53F19\tQ96GX9\nQ53F19\tQ6IEG0\nQ53F19\tQ9BY42\nQ53F19\tP61326\nQ53F19\tQ96IZ7\nQ53F19\tQ8TF76\nQ53F19\tO00148\nQ53F19\tP49756\nQ53F19\tQ10589\nQ53F19\tP31431\nQ53F19\tP34741\nQ53F19\tP83916\nQ53F19\tP63104\nQ53F19\tO75400\nQ53F19\tQ8TDD1\nQ53F19\tP09603\nQ53F19\tQ86WX3\nQ53F19\tQ00403\nQ53F19\tQ13352\nQ53F19\tQ13011\nQ53F19\tQ9BRP8\nQ53F19\tQ9BSM1\nQ53F19\tQ6SPF0\nQ53F19\tO60684\nQ53F19\tQ16630\nQ53F19\tP55198\nQ53F19\tP78563\nQ53F19\tO00422\nQ53F19\tQ9ULL5\nQ53F19\tP52298\nQ53F19\tQ09161\nQ53F19\tQ9BXP5\nQ53F19\tQ96PV6\nQ53F19\tQ9UHI6\nQ53F19\tQ9UKV3\nQ53F19\tQ86U42\nQ53F19\tQ13155\nQ53F19\tP34897\nQ53F19\tQ9UKS6\nQ53F19\tO14950\nQ53F19\tP53582\nQ53F19\tP20700\nQ53F19\tP68400\nQ53F19\tQ5JVF3\nQ53F19\tP19784\nQ53F19\tO00629\nQ53F19\tP68036\nQ53F19\tQ96A72\nQ53F19\tQ9Y5S9\nQ53F19\tP15692\nQ53F19\tP35637\nQ53F19\tQ00688\nQ53F19\tP98179\nQ53F19\tQ9P289\nQ53F19\tO00505\nQ53F19\tQ96K17\nQ53F19\tQ9H307\nQ53F19\tQ96J01\nQ53F19\tQ8NI27\nQ53F19\tQ13769\nQ53F19\tQ13838\nQ53F19\tQ86W42\nQ53F19\tQ96FV9\nQ53F19\tP38919\nQ53F19\tQ6I9Y2\nQ53F19\tP51148\nQ53F19\tP50395\nP52298\tQ9UH99\nP52298\tP04003\nP52298\tQ8N0X7\nP52298\tK7ELV2\nP52298\tP14735\nP52298\tQ5VV67\nP52298\tP31150\nP52298\tQ5DJT8\nP52298\tP52294\nP52298\tP09603\nP52298\tP53007\nP52298\tQ9NWV8\nP52298\tQ8N5P1\nP52298\tQ9ULL5\nP52298\tO94762\nP52298\tP39880\nP52298\tQ8IXH7\nP52298\tQ9H3P2\nP52298\tQ6NZY4\nP52298\tQ8WX92\nP52298\tQ9Y580\nP52298\tQ86VM9\nP52298\tQ9H814\nP52298\tP18615\nP52298\tQ09161\nP52298\tQ9BXP5\nP52298\tQ9UHI6\nP52298\tQ8NC51\nP52298\tO75822\nP52298\tQ9P289\nP52298\tO00505\nP52298\tP51148\nP52298\tP50395\nP52298\tP61927\nP52298\tO00505\nP52298\tQ13769\nP52298\tQ9H814\nP52298\tQ09161\nP52298\tQ9BXP5\nP52298\tP38919\nQ53F19\tO00505\nQ53F19\tQ13769\nQ53F19\tQ9H814\nQ53F19\tP38919\nQ53F19\tQ09161\nQ53F19\tQ9BXP5\nQ09161\tQ53F19\nP33724\tP59632\nP33724\tP59632\nP33724\tP59632\nP33724\tP59632\nP59632\tP33724\nP0DTC9\tP0DTC4\nP0DTC3\tP0DTC4\nP0DTD2\tP0DTC4\nQ14160\tB7Z2Y1\nQ14160\tQ14155\nQ14160\tQ7Z628\nP22460\tQ14160\nQ96DN2\tQ14160\nO00429\tQ14160\nQ99569\tQ14160\nQ96DL1\tQ14160\nB7Z2Y1\tQ14160\nQ14155\tQ14160\nQ9UQB3\tQ14160\nQ9BR11\tQ14160\nQ96DL1\tQ14160\nQ14155\tQ14160\nB7Z2Y1\tQ14160\nQ9ULI0\tQ14160\nP25100\tQ14160\nP53778\tQ14160\nQ9NYB5\tQ14160\nQ15311\tQ14160\nP33402\tQ14160\nQ9P0K1\tQ14160\nQ92953\tQ14160\nO15439\tQ14160\nP35222\tQ14160\nP48065\tQ14160\nQ14160\tP08581\nP22460\tQ14160\nO00429\tQ14160\nQ14160\tQ6UY11\nQ14160\tQ9ULJ7\nQ14160\tA6NIM6\nP81408\tQ14160\nQ9NQG5\tQ14160\nQ6UXZ0\tQ14160\nQ92502\tQ14160\nQ8TA94\tQ14160\nQ9Y6R1\tQ14160\nQ9Y466\tQ14160\nQ8TBB1\tQ14160\nP22460\tQ12959\nQ9UQB3\tQ12959\nQ12959\tQ96DL1\nQ12959\tQ15311\nQ12959\tQ96A65\nQ6ZTQ3\tQ12959\nQ12959\tQ14CM0\nQ12959\tQ9NYB5\nQ12959\tQ14524\nQ12959\tQ13224\nQ12959\tP85299\nQ12959\tQ9NVW2\nQ12959\tQ99569\nQ12959\tQ6DN90\nQ12959\tQ86W11\nQ12959\tA1L4L8\nQ12959\tQ15303\nQ12959\tQ96GG9\nQ96GL9\tQ12959\nQ86UD3\tQ12959\nO60333\tQ12959\nA1A5B4\tQ12959\nQ7Z628\tQ12959\nP0C2L3\tQ12959\nQ9NS75\tQ12959\nQ96SF7\tQ12959\nB7Z2Y1\tQ12959\nQ9NS75\tQ12959\nQ9P021\tQ12959\nQ7Z628\tQ12959\nQ99569\tQ96NW7\nQ9UQB3\tQ96NW7\nQ9UQB3\tQ96RT1\nQ99569\tQ96RT1\nO00192\tQ96RT1\nQ9BY21\tQ96RT1\nQ96DL1\tQ96RT1\nQ8NHY3\tQ96RT1\nP50804\tQ12959\nP24835\tQ12959\nP50804\tQ12959\nP24835\tQ12959\nP27228\tQ12959\nQ06093\tQ12959\nP06463\tQ12959\nP21735\tQ12959\nP30911\tQ12959\nP06427\tQ12959\nP0C213\tQ12959\nP03126\tQ12959\nP27962\tQ12959\nP54667\tQ12959\nP54667\tQ12959\nP09708\tQ12959\nP17386\tQ12959\nP36807\tQ12959\nP26554\tQ12959\nQ0A442\tQ12959\nP30910\tQ12959\nP26555\tQ12959\nP16717\tQ12959\nP50804\tQ12959\nP24835\tQ12959\nP21735\tQ12959\nP06427\tQ12959\nP27228\tQ12959\nP09708\tQ12959\nA3EX99\tQ96RT1\nA3EXD5\tQ96RT1\nP06427\tQ96RT1\nQ9IDV3\tQ96RT1\nQ1A244\tQ96RT1\nP03126\tQ96RT1\nP50804\tQ96RT1\nP0C9G5\tQ96RT1\nA3EXD4\tQ96RT1\nP89432\tQ96RT1\nP0C213\tQ96RT1\nQ14160\tP06427\nQ14160\tP03126\nQ14160\tP17589\nP0C213\tQ14160\nQ14160\tP0C222\nP06427\tQ14160\nP03126\tQ14160\nP0C213\tQ14160\nP27228\tQ14160\nP24835\tQ14160\nP04299\tQ14160\nP16717\tQ14160\nP50804\tQ14160\nP21735\tQ14160\nP0C222\tQ14160\nP03333\tQ14160\nQ09SZ7\tQ14160\nQ08089\tQ14160\nQ18LE1\tQ96RT1\nQ14500\tQ14160\nP48050\tQ14160\nP12814\tQ14160\nP35609\tQ14160\nQ08043\tQ14160\nO43707\tQ14160\nQ8IWK6\tQ12959\nQ86SQ6\tQ12959\nQ96PE1\tQ12959\nP22459\tQ12959\nQ14500\tQ12959\nQ14155\tQ14160\nP33402\tQ14160\nP53778\tQ14160\nQ99569\tQ14160\nQ99569\tQ14160\nQ14155\tQ14160\nP53778\tQ14160\nP33402\tQ14160\nP35222\tQ14160\nP59595\tP63165\nP59595\tP63165\nP59595\tP09651\nP59595\tP09651\nP59595\tP09651\nP0C6X7\tO75348\nP0C6X7\tO75348\nP0C6X7\tO75348\nP0C6X7\tO75348\nP0C6X7\tO75348\n',
      },
      {
        kind: 'annotation',
        name: 'COVID-19 KEGG pathways',
        fileName: 'HomoSapiens_Pathways_KEGG_PATHWAY_FILTERED.txt',
        text: 'Viral carcinogenesis\tQ00403,Q08043,Q14160,P04439,Q96EY1,Q92793,P06493,P35609,Q14653,P12814,P27348,Q7KZF4,P62258,O43707,P62191,P63104,Q12959\nInfluenza A\tP52294,P42224,P78406,Q92793,P53778,P52948,Q13838,Q14653,P08107,P52630,P0DMV8,O14980,P52292,Q86UT6,P00747,Q86U42\nHerpes simplex infection\tP19784,P42224,P62140,P67870,P04439,Q92793,P06493,P68400,P24928,Q14653,P52630,Q9UER7,Q86U32\nProximal tubule bicarbonate reclamation\tQ9Y6R1,Q9UBX3,P05023,P05026\n',
      },
      {
        kind: 'annotation',
        name: 'COVID-19 SMART domains',
        fileName: 'HomoSapiens_Protein_Domains_SMART_FILTERED.txt',
        text: 'SM00265:BH4\tQ07817,Q92843,P10415\nSM00033:CH\tQ14315,P12814,Q08043,O43707,P13797,Q8NHY3,P35609,Q14155\nSM00361:RRM_1\tP11940,Q14011,Q13310,P19338,P38159\nSM00150:SPEC\tP12814,Q08043,P15924,O43707,P35609\nSM00088:PINT\tQ9UNM6,Q15008,Q14152,O00231\nSM00101:14_3_3\tP27348,P62258,P63104\nSM00948:SM00948\tP25787,O14818,P60900\nSM00557:IG_FLMN\tQ14315,Q7Z4H8,Q6UW63\nSM00490:HELICc\tQ9NR30,Q8TDD1,Q9BUQ8,Q9UHI6,O00148,P38919,Q13838,O94762\nSM00487:DEXDc\tQ9NR30,Q8TDD1,Q9BUQ8,Q9UHI6,O00148,P38919,Q13838,O94762\nSM00244:PHB\tP27105,P35232,Q99623\nSM00271:DnaJ\tQ8IXB1,Q99615,P31689,Q96EY1,O60884\n',
      },
      {
        kind: 'annotation',
        name: 'COVID-19 InterPro domains',
        fileName: 'HomoSapiens_Protein_Domains_INTERPRO_FILTERED.txt',
        text: 'P-loop containing nucleoside triphosphate hydrolase\tQ9UM54,P35998,Q9NVI7,Q9ULI0,Q5T9A4,Q9HCE1,Q9NTJ3,P11586,P62195,Q02224,P33176,Q9H078,P38606,O00429,O15439,O00148,Q00839,Q9Y265,P38919,Q92900,Q96MM7,O14656,O94762,Q05639,Q5JWF2,Q9NR30,Q8TDD1,Q9UHI6,Q9BUQ8,Q13838,P51148,P25205,P06576,P33527,O95347,Q9NU22,P25705,O60333,P62191,Q9Y276,Q12959\nEF-hand-like domain\tQ08043,P42224,P60660,Q15293,Q07820,P35609,Q9Y680,O14950,P0DP23,P12814,P19105,P52630,O75746,Q9H4F8,Q96AY3,O43707,O43852,Q14257,P13797\nApoptosis regulator,Bcl-2,BH4 motif,conserved site\tQ07817,Q92843,P10415\nATPase,F1/V1/A1 complex,alpha/beta subunit,C-terminal\tP06576,P38606,P25705\nApoptosis regulator,Bcl-2 protein,BH4\tQ07817,Q92843,P10415\nDNA/RNA helicase,DEAD/DEAH box type,N-terminal\tQ9NR30,Q8TDD1,Q9BUQ8,Q9UHI6,O00148,P38919,Q13838,O94762\nFilamin/ABP280 repeat-like\tQ14315,O75592,Q7Z4H8,Q6UW63\nChaperone DnaJ\tP31689,Q96EY1,O60884\nCalponin homology domain\tQ14315,P12814,Q08043,O43707,P13797,Q8NHY3,P35609,Q14155\nRNA recognition motif domain,eukaryote\tP11940,Q14011,Q13310,P19338,P38159\nATPase,AAA-type,conserved site\tP35998,Q9ULI0,P62191,Q9Y276,P62195\nPeptidase M16,zinc-binding site\tQ10713,O75439,P14735\nKinase associated domain 1 (KA1)\tQ9P0L2,Q7KZI7,P27448\nATPase,alpha/beta subunit,N-terminal\tP06576,P38606,P25705\nHeat shock protein DnaJ,cysteine-rich domain\tP31689,Q96EY1,O60884\nATPase,F1/V1/A1 complex,alpha/beta subunit,nucleotide-binding domain\tP06576,P38606,P25705\nATPase,alpha/beta subunit,nucleotide-binding domain,active site\tP06576,P38606,P25705\nSpectrin/alpha-actinin\tP12814,Q08043,P15924,O43707,P35609\nImmunoglobulin E-set\tQ14315,Q96DL1,O75592,P08581,Q14500,Q7Z4H8,P48050,P61916,Q6UW63\nEF-Hand 1,calcium-binding site\tP0DP23,P12814,P19105,O75746,Q9H4F8,Q15293,Q96AY3,O43707,O43852,P13797,Q14257,Q9Y680,O14950\nEF-hand domain\tQ08043,P60660,Q15293,P35609,Q9Y680,O14950,P12814,P0DP23,P19105,O75746,Q96AY3,O43707,O43852,P13797,Q14257\n26S proteasome subunit P45\tP35998,P62191,P62195\nG-protein beta WD-40 repeat\tO43818,P53621,K7ELV2,P78406,P35606,P63244,Q16576,Q96J01\n14-3-3 protein\tP27348,P62258,P63104\n14-3-3 protein,conserved site\tP27348,P62258,P63104\n14-3-3 domain\tP27348,P62258,P63104\nApoptosis regulator,Bcl-2,BH3 motif,conserved site\tQ07817,Q07820,P10415\nProteasome A-type subunit\tP25787,O14818,P60900\nProteasome,alpha-subunit,N-terminal domain\tP25787,O14818,P60900\nChaperone DnaJ,C-terminal\tP31689,Q96EY1,O60884\nHSP40/DnaJ peptide-binding\tP31689,Q96EY1,O60884\nHelicase,C-terminal\tQ9NR30,Q8TDD1,Q9BUQ8,Q9UHI6,O00148,P38919,Q13838,O94762\nCation efflux protein\tQ8NEW0,Q6NXT4,Q6PML9\nCation efflux protein transmembrane domain\tQ8NEW0,Q6NXT4,Q6PML9\nFilamin/ABP280 repeat\tQ14315,Q7Z4H8,Q6UW63\nSpectrin repeat\tP12814,Q08043,O43707,P35609\nHelicase,superfamily 1/2,ATP-binding domain\tQ9NR30,Q8TDD1,Q9BUQ8,Q9UHI6,O00148,P38919,Q13838,O94762\nBromodomain,conserved site\tP25440,Q9ULI0,Q92793,O60885\nAnoctamin/TMEM 16\tA1A5B4,Q32M45,Q4KMQ2\nRNA helicase,ATP-dependent,DEAD-box,conserved site\tQ8TDD1,Q9BUQ8,Q9UHI6,P38919\nMitochondrial carrier protein\tP53007,O75746,Q9BQT8,Q9H936\nTetratricopeptide TPR-1\tQ99615,O94826,P31948,O43765\nBand 7 protein\tP27105,P35232,Q99623\nWD40-repeat-containing domain\tK7ELV2,Q9ULV4,P78406,P35606,Q86W42,Q16576,Q96JC1,O43818,B7Z2Y1,P53621,Q6P2E9,Q9H270,Q12770,P63244,Q96J01\nvon Willebrand factor,type A\tQ9NWV8,P05556,P12109,P12956,Q9NU22,P13010,P10155\nDnaJ domain\tQ8IXB1,Q99615,P31689,Q96EY1,O60884\nProteasome/cyclosome,regulatory subunit\tQ13200,Q99460\nApoptosis regulator,Bcl-2/ BclX\tQ07817,P10415\nTranscription factor TFIIB,cyclin-like domain\tQ00403,Q92994\nInsulin-induced protein family\tO15503,Q9Y5U4\nCoatomer,WD associated region\tP53621,P35606\nKu70/Ku80 C-terminal arm\tP12956,P13010\nKu70/Ku80,N-terminal alpha/beta\tP12956,P13010\nKu70/Ku80 beta-barrel domain\tP12956,P13010\nRab GDI protein\tP50395,P31150\nATPase,F1 complex beta subunit/V1 complex,C-terminal\tP06576,P38606\nMago nashi protein\tP61326,Q96A72\nClathrin/coatomer adaptor,adaptin-like,N-terminal\tQ9UBF2,O00203,P53618',
      },
      {
        kind: 'annotation',
        name: 'COVID-19 GO biological process',
        fileName: 'HomoSapiens_Gene_Ontology_GOTERM_BP_DIRECT_FILTERED.txt',
        text: 'negative regulation of anoikis\tP06731,Q07817,P05556,Q07820,P10415\nT cell receptor signaling pathway\tP35998,P48556,Q13200,P25787,Q99460,P62195,Q9UNM6,O14818,Q15008,P51665,P60900,O00231,P62191\nregulation of cellular response to heat\tQ99615,P08107,K7ELV2,P62258,Q9UL15,O95816,P0DMV8,P78406,Q92793,P52948\nRNA secondary structure unwinding\tQ9NR30,Q8TDD1,Q9BUQ8,Q9UHI6,O00148,P38919,Q13838\nSRP-dependent cotranslational protein targeting to membrane\tP60866,P08708,P61254,P62269,P39019,P42677,P61927,P62753,Q9Y3U8,P46782\npositive regulation of establishment of protein localization to telomere\tP40227,P17987,P48643,Q99832\nFc-epsilon receptor signaling pathway\tP35998,P48556,Q13200,P25787,Q99460,P62195,P0DP23,Q9UNM6,O14818,Q15008,P51665,P60900,O00231,P62191\nmRNA transport\tQ09161,O14980,Q9Y5S9,Q53F19,P09651,Q96A72,Q9UN86\nnegative regulation of transcription elongation from RNA polymerase II promoter\tQ8WX92,Q9H3P2,P18615,O94762\nnuclear export\tQ09161,Q9H814,P52298,P09651\nresponse to hydrogen peroxide\tO00629,Q13443,P22460,P42224,Q9P289,P09601,P10415\nintracellular protein transport\tQ9UM54,Q9H173,Q9ULJ7,Q14974,O00203,P35606,O14964,Q96JC1,P53621,O95373,Q9UBF2,Q9H270,O15397,O14980,O43592,P53618\nrRNA processing\tP60866,Q9NR30,P39019,P62753,P61927,P57678,O43818,P08708,P61254,Q9NU22,P62269,P42677,P38919,P46782,Q9Y3U8\nrelease of cytochrome c from mitochondria\tQ07817,Q16548,Q3ZCQ8,O00429,P10415\npositive regulation of translation\tP11940,Q9BRP8,Q14011,Q6PKG0,P98179,P38919,Q13838\nfocal adhesion assembly\tP12814,Q08043,P35609,Q14155,P10415\ncalcium activated phosphatidylserine scrambling\tA1A5B4,Q32M45,Q4KMQ2\ngene silencing by RNA\tP30876,Q09161,Q7KZF4,P11940,K7ELV2,P52298,O15397,P78406,P52948,P24928\nnegative regulation of programmed cell death\tP02768,Q96EY1,O76024,P62195\nprotein folding in endoplasmic reticulum\tQ8N766,Q8IXB1,Q86YB8,P11021\npositive regulation of viral genome replication\tP62937,P11940,Q6PKG0,Q9P035,P78563\nprotein complex assembly\tP52907,O14745,Q9BSJ2,O43292,Q9Y375,Q9NU22,P30153,Q92793,P06493,O00505\nmitochondrion organization\tP06576,P35232,Q5VV67,O00429,Q96EY1,P09874,Q9Y276,Q99623\nT cell differentiation in thymus\tP61769,Q96EY1,P62753,P35222,P10415\nintra-Golgi vesicle-mediated transport\tP53621,Q9UBF2,P35606,P39880,P53618\nprotein homotetramerization\tQ9H773,Q96GX9,O00429,P34897,Q14500,P35609,P14735\ncalcium activated galactosylceramide scrambling\tA1A5B4,Q32M45,Q4KMQ2\ncalcium activated phosphatidylcholine scrambling\tA1A5B4,Q32M45,Q4KMQ2\nviral entry into host cell\tP08107,P27487,P05556,P15151,P0DMV8,Q9BYF1,P15144,O00505,P14735\nDNA duplex unwinding\tQ13283,P12956,P13010,Q9Y265,P25205,O94762\nplatelet degranulation\tP0DP23,P12814,P05155,P02768,O15439,P15692,O43707,P00747,P35609\nregulation of nucleic acid-templated transcription\tP12814,P35637,O43707,P35609\ncell proliferation\tO15503,P68036,Q14160,Q13561,P08581,P13010,Q16576,P06493,Q15303,P25100,Q07817,P09603,Q9BXP5,Q6PKG0,O95071,P42677,Q7L590,P12004,Q10589,P10415\nosteoblast differentiation\tP35998,Q9NR30,Q7KZF4,P06576,P35232,P12109,Q00839,J3QSV6,P38159\nintrinsic apoptotic signaling pathway in response to DNA damage\tQ07817,Q16548,Q92843,Q07820,P09601,P10415\nregulation of DNA recombination\tP52294,P52292,Q96FV9\npositive regulation of histone H3-K4 methylation\tP26358,P35222,Q9H3P2,P18615\ntransport\tP53007,Q9Y6R1,P02768,Q9UBX3,P52948,Q15818,Q9UN86,Q9NZJ7,Q15311,P46379,P33527,P38606,Q13283,Q00325,Q02978,Q9Y3A6,Q13224,Q9H936,P48065\npositive regulation of intrinsic apoptotic signaling pathway\tO14745,Q07817,O00429,P63244,P10415\n7-methylguanosine mRNA capping\tP30876,Q09161,P52298,Q53F19,P24928\nSREBP signaling pathway\tO15503,Q9Y5U4,Q12770\nIRES-dependent viral translational initiation\tP26599,Q14152,O00303\nextrinsic apoptotic signaling pathway in absence of ligand\tQ07817,Q16548,Q92843,Q07820,P10415\nribosomal small subunit assembly\tP08708,P39019,P42677,P46782\nubiquitin-dependent protein catabolic process\tP35998,P68036,P46379,Q9UNM6,P25787,O14818,Q9NXK8,O95071,P60900,O00231,P63279,Q9NVW2\nregulation of translational initiation\tQ09161,P52298,O75822,Q14152,O00303\nestablishment of integrated proviral latency\tP62937,P12956,P13010\nresponse to endoplasmic reticulum stress\tQ8IXB1,Q13438,P57088,Q9Y4L1,P16615,O76024,Q9BS26\nchaperone-mediated protein folding\tQ96AY3,P68400,O14656,Q9Y680,Q00688\ngastrulation\tP37173,P63244,P62753,P09622\nfibrinolysis\tP07355,P00750,P05155,P00747\nnegative regulation of mRNA splicing,via spliceosome\tO00422,P26599,Q9UKV3,P38159\nRNA processing\tQ13310,Q8TDD1,Q9UHI6,P98179,Q96FV9,Q00839,P78563,Q86U42\nnegative regulation of apoptotic process\tQ9NVI7,P31689,P02768,Q96GX9,Q92843,Q96EY1,P62753,Q86W42,P06493,Q07820,Q15303,P06731,Q07817,Q16548,P46379,Q9Y466,P11021,P15692,Q5JVF3,Q99623,P10415,P63104\nnegative regulation of intrinsic apoptotic signaling pathway\tQ07817,Q92843,Q07820,P10415\nprotein targeting\tP27348,Q13438,P62258,Q96RT1,P63104\nregulation of smooth muscle cell proliferation\tP12956,P13010,P35222\npositive regulation of RNA polymerase II transcriptional preinitiation complex assembly\tP35998,P62191,P62195\nresponse to hypoxia\tP22460,P00750,P37173,P27487,O00469,Q12770,P15692,Q92793,P09601,O43707,P05026\nresponse to drug\tQ9UM54,Q5JWF2,P42224,P13010,P06493,Q92820,P35222,P61769,P37173,P33527,O14980,P40939,O15439,P28300,P05023,P10415\nmembrane repolarization during cardiac muscle cell action potential\tP62258,P05023,P05026\nendoplasmic reticulum mannose trimming\tQ13438,Q9BZQ6,Q9NYU1\npositive regulation of potassium ion transport\tP33176,P35609,Q12959\npositive regulation of cellular component movement\tP39019,P15692,O43707\nresponse to unfolded protein\tP34932,Q96CS3,P31689,O95757,Q9BS26\ncellular protein modification process\tP00750,P68036,P38435,O00469,Q86YB8,P22314,P28300,P63279\nprotein peptidyl-prolyl isomerization\tQ6UX04,P62937,Q96AY3,Q9Y680,Q00688\nmuscle contraction\tQ9NYL9,P0DP23,P19105,Q08043,P60660,Q14500,P50402,Q86U42,O14950\ntype I interferon signaling pathway\tP42224,P52630,P04439,P05161,Q10589,Q14653\npositive regulation of protein ubiquitination\tQ9NV92,P68036,Q13155,Q96EY1,P11021,O76024\ngluconeogenesis\tP04406,P53007,O75746,Q9UBX3,Q02978\nmitotic nuclear envelope disassembly\tK7ELV2,P78406,P06493,P50402,P52948\npositive regulation of mesenchymal cell proliferation\tP37173,P42224,P15692,P35222\nprotein targeting to plasma membrane\tP07355,P19105,Q92953,P35613,O14950\npositive regulation of protein targeting to mitochondrion\tP19784,P48556,Q8TAD8,P68036,Q9H270,P55786,Q9Y265\nnuclear pore complex assembly\tP57088,Q9NQC3,P52948\npositive regulation of DNA-templated transcription,elongation\tQ13769,Q96FV9,Q13838\nendoplasmic reticulum unfolded protein response\tO76061,P11021,Q9BZQ6,Q9NYU1,O76024\nER-associated misfolded protein catabolic process\tQ99470,Q9NYU1,O14656\nrelaxation of cardiac muscle\tP05023,P16615,P05026\nproteasome assembly\tP48556,Q9UNM6,O00231\nregulation of cardiac muscle cell contraction\tQ14524,P05023,P63165\nlysine catabolic process\tQ9UDR5,Q9BQT8,P09622\npotassium ion import\tQ14500,P48050,P05023,P05026\nproteolysis involved in cellular protein catabolic process\tP25787,O14818,P11021,P60900,P14735\nsubstantia nigra development\tP0DP23,P27348,O15027,P62258,P11021\nestablishment or maintenance of epithelial cell apical/basal polarity\tQ7KZI7,Q96RT1,Q12959\nmicrotubule cytoskeleton organization\tQ9P0L2,P04406,Q96N67,Q7KZI7,P06493,P27448\npositive regulation of protein insertion into mitochondrial membrane involved in apoptotic signaling pathway\tP27348,P62258,P10415,P63104\n',
      },
      {
        kind: 'annotation',
        name: 'COVID-19 GO molecular function',
        fileName: 'HomoSapiens_Gene_Ontology_GOTERM_MF_DIRECT_FILTERED.txt',
        text: 'poly(A) RNA binding\tP62753,J3QSV6,Q16630,Q13283,P62269,O00148,Q02978,O43707,Q00839,Q86WX3,Q13151,P68036,Q9BRP8,P17987,P39019,Q8TDD1,Q6NZY4,P11940,Q9UKV3,P62258,P56270,P16402,P67809,P61254,P63244,Q8NC51,P38159,P40227,Q9Y580,Q14974,P12956,P09874,Q00688,P78563,P30876,Q7KZF4,Q6PKG0,P63279,Q92900,P63104,Q7L8L6,Q8N5P1,Q9BUQ8,Q86VM9,Q14152,P63165,Q9UN86,Q13838,Q8WTT2,O43818,P50395,Q9BXP5,O75400,P61326,Q96A72,Q9Y3U8,P07355,Q09161,Q9Y5A9,P31948,P18615,Q6PJT7,Q9H307,Q5VV67,Q8NCA5,P09651,P15924,Q13310,P20290,P52298,P22314,P49756,P04439,P24928,P26599,P35637,Q9Y5S9,Q9NW13,P19338,P62191,Q15365,P04003,O75534,Q9ULX6,Q9HCE1,Q53F19,O00422,P52292,Q14739,P98179,P38919,Q9UKA9,P46782,Q86U42,Q16891,P60866,Q8TAD8,Q9NR30,P13010,P08708,P62937,P49368,Q14011,P25705,Q7KZI7,P42677,P08621,Q9NQC3,Q86U32,O75940,Q10589\nprotein binding\tQ9P021,Q13200,Q96GX9,Q13769,Q5H9R7,Q3ZCQ8,Q99988,Q86VR2,P62753,Q9NTJ3,P0DP23,P61769,Q9BSJ2,O43292,P62269,Q8TBB1,P59998,P50402,Q8IXH7,Q00839,P34741,Q99569,Q13561,Q07820,P55198,Q13352,Q4KMQ2,Q9NZJ7,Q96GG9,Q9H3K2,Q07817,Q9UKV3,P52630,P62258,Q86YB8,Q8NC51,P35998,P40227,Q00403,P26358,Q96PE1,P35606,Q92793,Q96FV9,Q15303,P09874,P35609,Q9Y680,Q00688,P78563,Q13443,Q15311,P27348,Q6PKG0,Q12789,P11021,Q92900,P63104,Q9ULJ7,Q8WX92,Q7L8L6,P35613,Q9BUQ8,Q9UBX3,Q9P035,P25100,Q13838,Q9NQG5,P06576,Q9Y5U4,Q13438,Q9BXP5,O60333,P55085,Q13224,Q9NS75,P53618,Q09161,O14745,Q9P0K1,Q9UL15,Q9Y5A9,P31948,P30153,Q9H773,Q6PJT7,O75592,Q8NCA5,Q9Y3A6,P57088,O00303,P05023,P09651,P05026,Q9NZ01,P52294,Q9BQ95,Q96CW5,O95619,Q13310,P20290,P52298,Q8N0Z6,P49756,P14735,P24928,P37173,Q9H270,Q9Y5S9,P51665,O00192,P60900,Q96HW7,P00747,P19338,Q15365,Q12959,P27105,Q9BSM1,P25787,P04003,Q9UQB3,Q14CM0,P81408,P67870,Q9HCE1,P68400,Q8IXB1,P33176,O00422,P00750,Q96IZ7,P08107,P46379,P83916,Q8NEM0,Q96DZ1,P48643,O00429,P52292,P05161,O43765,P46782,Q99623,Q9UKS6,Q9NR30,O75348,P52948,P51148,Q8N0X7,P49368,Q99615,P05155,O15397,P0DMV8,P08621,Q6ZTQ3,Q9NQC3,O43852,Q10589,O76024,P25440,P04406,Q9ULV4,Q14524,Q96EY1,P53778,P35247,P09601,Q16630,Q5BJF2,Q9H845,O15027,Q13283,P15151,Q9NXK8,O00148,O95071,O95070,P53396,Q7L590,O43707,O14656,P10415,Q05639,Q86WX3,P68036,Q9BRP8,Q9Y6R1,P05556,O75822,P17987,Q13155,P39019,Q00765,Q13011,Q15293,P48668,O60884,Q6NZY4,O60885,O60684,P11940,P61254,P56270,P67809,Q9NU22,Q15008,P63244,Q96HR9,P31150,P48065,P38159,Q9BS26,Q9H173,Q9UM54,P34932,Q96A65,Q14974,O43633,Q96IV0,P12956,Q14160,Q96PV6,P16615,P35222,Q99832,P62195,P12814,P30876,P19105,Q9H078,Q02224,P52907,Q7KZF4,P35232,Q9Y375,O95816,Q14257,P63279,Q5JWF2,O94826,Q8N5P1,Q86VM9,Q14152,P07195,Q9H3P2,P63165,Q9UN86,Q14155,Q6DN90,Q14653,Q96NW7,P09603,P50395,Q9H814,O95347,P40939,P31431,O75400,Q8IWR1,Q9UER7,P61326,Q96A72,P07355,P31689,P42224,K7ELV2,P39656,Q16576,Q9BYF1,P11586,P18615,P27487,Q9NWV8,Q7Z5G4,Q9H8W4,P13804,Q9Y6M9,Q8N8U2,Q9Y265,Q96SF7,O00629,Q5SW79,P15924,P62140,P22459,P22314,Q9UHI6,Q99471,Q9UH99,P04439,Q96RT1,O14950,Q96S52,P22460,P26599,O14818,P20700,P35637,Q12770,Q8NI27,Q9H4F8,Q86UT6,P15692,Q9P289,P61158,P62191,Q9Y276,P19784,Q8IV08,Q96CS3,O15503,P08581,Q92843,O75534,P06493,Q9ULX6,P61916,Q53F19,O14964,O95373,Q9NV92,Q16548,Q96F46,O00124,Q14739,P28300,O75934,P98179,P48050,P38919,P12004,Q86U42,Q16891,P60866,Q8IWF2,Q6I9Y2,Q8TAD8,Q08043,Q8TF76,P02768,P60660,P13010,P34897,P57678,Q9NYU1,O00505,P27448,P25205,Q14315,Q6P2E9,P62937,O60762,Q14011,Q92552,O14980,P25705,Q7KZI7,P42677,Q96K17,Q5JVF3,O00231,Q86U32,O75940,P49069\nprotein domain specific binding\tQ14974,O43633,Q14524,Q9UHI6,P67870,P35609,O14964,P0DP23,P27348,Q9H270,P62258,O14980,P11021,P05023,P00747,P63104\nion channel binding\tP0DP23,P12814,P27348,Q08043,P62258,Q14524,O43707,P35222,P63165,P35609,Q12959\nHsp70 protein binding\tQ8IXB1,P31689,P46379,P31948,Q96EY1,P06493\nglycoprotein binding\tQ8IWF2,P61769,P00750,Q13438,Q96DZ1,P11021,Q9BYF1,P14735\nTBP-class protein binding\tP35998,Q00403,Q92994,P62191,P62195\nestrogen receptor binding\tP35637,Q8TDD1,P09874,P12004,P35222,Q99623\nmisfolded protein binding\tQ8IXB1,P46379,P11021,O14656\nprotein transporter activity\tO00629,P52294,O60684,O95373,Q14974,O15397,P52292,O00505\nBH3 domain binding\tQ07817,Q07820,P10415\nactin filament binding\tQ14315,P12814,Q9UM54,P47756,Q9ULV4,P59998,O43707,P61158,P13797,Q8NHY3,P35609\npoly(A) binding\tP11940,Q6PJT7,Q13310,P38919\nidentical protein binding\tP04406,P42224,Q96GX9,P67870,P09874,Q99832,P35609,P61769,Q9H773,P11310,P27487,Q8NEM0,O95816,O00429,O00148,Q7L590,P12004,P10415,P63104,P02768,Q13561,Q9UH99,P34897,P07195,Q13838,Q14653,P06731,Q07817,O14818,P52630,P35637,P15692,Q9P289,P19338,P38159\nRan GTPase binding\tO95373,Q14974,O15397,O14980,O43592\nintegrin binding\tQ13443,P12814,Q08043,Q9P0K1,P05556,O43707,Q96RT1,Q08431,P35609\nproteasome-activating ATPase activity\tP35998,P62191,P62195\ndolichyl-phosphate-mannose-protein mannosyltransferase activity\tO60762,Q9Y673,Q99470\ncytoskeletal protein binding\tP07355,Q14315,Q9UKS6,O14656,P35609,Q12959\noxidoreductase activity,acting on the CH-CH group of donors\tQ9NZ01,Q9H845,P11310,Q14739\nvirus receptor activity\tP08107,P27487,P05556,P15151,P0DMV8,Q9BYF1,P15144,P14735\nprotein N-terminus binding\tP19784,Q96A65,P27348,Q9UER7,O43707,P09874,P68400,Q99623\nprotein homodimerization activity\tP27105,P42224,Q92843,O76061,P09601,Q15303,P35609,P12814,Q16548,P83916,P27487,P15151,O00429,O43707,Q96SF7,P10415,Q08043,P39019,Q07820,P14735,Q14653,Q4KMQ2,P06731,Q07817,P09603,P63244,P15692,P51665,Q9UER7,Q9P289,Q10589,Q6UY11\nNAD binding\tP04406,P40939,P07195,P09622,P09874\nprotein kinase binding\tQ05639,Q13151,P62140,Q14524,Q9UL15,P39019,Q96EY1,P62753,P09874,P35222,Q14155,P0DP23,Q07817,P22460,P62269,Q9UER7,P05023,P63104,Q12959\nrRNA binding\tQ9NR30,Q7L8L6,P62269,P61927,P46782\nhistone deacetylase binding\tP35232,P08107,P62258,P0DMV8,P52292,Q9UHI6,Q9Y466,Q9ULX6,P09874\ntelomeric DNA binding\tP12956,P13010,P19338,Q92900\nRNA cap binding\tQ09161,Q6PKG0,P52298\nchromatin binding\tP25440,P26358,Q8N0Z6,Q9ULI0,P67870,P34897,Q92793,P06493,O60885,Q9H3P2,P18615,P30876,Q8WTT2,P83916,P67809,Q92900,P12004,Q6PML9,P38159\ncation transmembrane transporter activity\tQ8NEW0,Q6NXT4,Q6PML9\nphospholipid scramblase activity\tA1A5B4,Q32M45,Q4KMQ2\npeptidyl-prolyl cis-trans isomerase activity\tQ6UX04,P62937,Q96AY3,Q9Y680,Q00688',
      },
      {
        kind: 'annotation',
        name: 'COVID-19 GO cellular component',
        fileName: 'HomoSapiens_Gene_Ontology_GOTERM_CC_DIRECT_FILTERED.txt',
        text: 'membrane\tP04406,Q13200,O00203,P62753,Q9NXS2,P09601,Q16630,J3QSV6,P61769,P53621,Q9BSJ2,O43292,P62269,O00148,O95071,P53396,Q2PZI1,P50402,Q00839,Q8IXH7,Q8TEQ8,O14656,P10415,P05556,Q13561,Q13155,P39019,Q8TDD1,Q13011,O60884,Q9BZQ6,Q07820,P29122,Q13352,Q4KMQ2,Q9NZJ7,Q9P2E5,O60684,P11940,Q07817,P62258,P61254,Q9NU22,P38159,Q8NC51,Q9UM54,P35998,Q96A65,Q14974,O43633,P12956,P09874,P16615,P35222,P62195,P30876,Q02224,Q7KZF4,P27348,Q15311,P35232,Q9Y673,Q6PKG0,Q12789,P11021,Q5JWF2,O94826,P35613,Q14152,P07195,Q6DN90,P06576,P50395,P09603,P33527,O75400,Q9Y3U8,P53618,P07355,O14745,P31689,Q9H488,Q99519,Q9UL15,P39656,P30153,Q9Y6E2,Q99460,Q96S66,P11586,P27487,O75592,Q9H307,O00303,Q9Y265,P05023,P09651,P05026,P51570,Q96CW5,Q9UHI6,P04439,Q99470,Q7Z2K6,Q96S52,P26599,P20700,P47756,Q00325,P15692,P51665,P61158,P62191,P19338,Q15365,P27105,P06493,P33176,Q8IXB1,O95373,P46379,O15439,Q14739,P52292,O00429,P38919,O43765,P46782,Q99623,Q16891,P60866,Q9NR30,Q9C0B5,P60660,P13010,P57678,P25205,P08708,Q6P2E9,P62937,Q99615,Q8NEW0,O60762,P38435,P12109,Q9UNM6,O14980,P25705,Q7KZI7,Q9Y4L1,O43852,O00231,Q08431,P49069,Q6NXT4,Q10589\nnucleoplasm\tP48556,Q13200,Q13769,Q5H9R7,Q3ZCQ8,P53778,P62753,Q16630,Q9NTJ3,P0DP23,Q9BSJ2,P62269,O00148,O95071,P53396,Q7L590,Q00839,Q8IXH7,Q96J01,Q86WX3,Q13151,Q9BRP8,P39019,P39880,Q6NZY4,Q86W42,Q07820,O60885,Q13352,O60684,Q6IEG0,Q9UKV3,P52630,P67809,Q15008,Q9H936,P38159,Q9UM54,P35998,Q00403,Q9Y580,Q14974,P26358,Q14160,P12956,Q92793,P09622,Q96FV9,Q15303,P09874,P35222,P62195,P78563,P30876,P35232,Q6PKG0,Q12789,Q96MM7,Q92900,P63279,O94762,P63104,Q8WX92,Q86VM9,Q9BUQ8,Q92994,Q8N5P1,Q9H3P2,Q9NVW2,P63165,Q14653,Q13838,Q9NQG5,O43818,Q9BXP5,Q9H814,O95347,Q9Y466,O75400,O43592,Q9UER7,P61326,Q09161,P42224,Q99460,Q16576,P18615,Q6PJT7,Q9NWV8,Q5VV67,Q9H307,Q9Y265,P09651,O00629,P52294,Q9BQ95,O95619,P62140,P52298,Q8N0Z6,P49756,Q9UHI6,Q96RT1,P14735,P24928,P26599,P20700,O14818,P35637,Q9Y5S9,Q8NI27,P51665,Q96HW7,P60900,P62191,P19338,Q15365,P19784,Q7Z4H7,P25787,P67870,P06493,Q9ULX6,P68400,O00422,O95373,P08107,P83916,P46379,Q8NEM0,P52292,O75934,P98179,P38919,P12004,P05161,P46782,Q86U42,P60866,Q6I9Y2,Q9UKS6,Q9NR30,Q8TAD8,P13010,P57678,O00505,P52948,P25205,P08708,Q6P2E9,Q99615,Q14011,Q9UNM6,O15397,O14980,P0DMV8,P42677,P08621,Q86U32,O00231\npseudopodium\tP12814,Q08043,P55085,O43707,P35609\nbrush border\tP12814,P19105,Q08043,P47756,P60660,O00429,O43707,P61158,O14950\nnuclear matrix\tO95619,P20700,O75400,O43592,P60900,Q9ULX6,Q96FV9,Q9Y265,Q99623,Q13838\nzona pellucida receptor complex\tP49368,P17987,P48643,Q99832\nextracellular matrix\tP07355,P60866,P40227,P04406,P15924,P60660,P39019,P29122,P08708,P06576,P00750,P12109,P62269,P40939,P25705,P11021,Q00839,Q08431,P46782\ncytosolic small ribosomal subunit\tP60866,P08708,P62269,P39019,P42677,P62753,P46782\nactin filament\tP12814,Q08043,Q9H270,P47756,Q7KZI7,Q96EY1,P13797,P35609\ncytosolic proteasome complex\tP35998,P62191,P14735,P62195\nsarcolemma\tP07355,Q14315,P05556,P12109,Q14524,P35613,P05023,P05026,Q12959\nvesicle\tP07355,P0DP23,P27105,P33176,P50395,O14745,Q8NEW0,P04406,Q13561,Q9ULV4,P60660\nplatelet alpha granule lumen\tP12814,P05155,P02768,P15692,O43707,P00747,P35609\nperinuclear region of cytoplasm\tP07355,Q9UM54,P27105,O14745,P04406,P31689,P42224,Q9UL15,O76061,P62753,P09601,P16615,P35222,P33176,Q9NV92,P08107,O00429,O43707,Q5JWF2,P05556,Q99569,P25205,Q8NEW0,P09603,P22460,P35637,P0DMV8,P63244,Q9P289,Q8NC51,Q12959\nspliceosomal complex\tP49756,O00148,O75934,P08621,Q9NW13,Q9UKA9,O75940,P09651,Q13838\nendoplasmic reticulum quality control compartment\tQ13438,Q96DZ1,Q9BZQ6,Q9NYU1\nmembrane raft\tP07355,P27105,O14745,P22460,P37173,P27487,P05556,P35613,P31431,P35052,Q9BYF1,P07195,Q10589,Q12959\nZ disc\tQ14315,P12814,P19105,P22460,Q08043,P47756,Q14524,O43707,P35222,P35609,O14950\nCOPI vesicle coat\tP53621,Q9UBF2,P35606,P53618\nsmall ribosomal subunit\tP60866,P62269,P63244,P62753,P46782\nmitochondrial nucleoid\tP06576,Q9NVI7,Q7L8L6,P40939,P34897,Q96EY1\ncell surface\tP07355,P05556,Q96PE1,Q14524,P08581,P04439,Q9BYF1,P29122,P14735,Q4KMQ2,Q13443,P06576,P00750,P35232,P27487,P15151,P31431,P11021,P15692,Q13224,Q00839,P00747,Q10589,P34741,Q9BS26,Q99623\nCRD-mediated mRNA stability complex\tP67809,O75534,Q00839\nendoplasmic reticulum-Golgi intermediate compartment\tO95070,P11021,Q9Y3A6,P15144,Q9NYU1,Q9BS26,P53618\nmicrotubule\tP40227,Q5SW79,Q96CW5,Q7Z4H7,P17987,Q13561,Q14152,Q99832,P49368,P33176,Q02224,Q9BSJ2,P48643,O00429,O60333,P50402,Q12959\nendocytic vesicle\tQ9UM54,P33176,Q9H270,P27487,P35247,P51148\ncytoplasmic stress granule\tP11940,Q14011,Q13310,Q13283,P67809\nproteasome regulatory particle,lid subcomplex\tP48556,Q9UNM6,O00231\nproteasome core complex,alpha-subunit complex\tP25787,O14818,P60900\nnuclear proteasome complex\tP35998,P62191,P62195\nmitochondrial matrix\tQ96EY1,P34897,O75439,Q07820,Q15303,P09622,Q8IZ52,Q9UDR5,Q07817,P06576,P11310,Q5JRX3,Q9BW92,Q10713,P25705,P13804,Q6L8Q7\ncytoplasmic mRNA processing body\tP35998,Q6P2E9,P25787,Q9Y5A9,P60900,Q9HCE1,Q92900\nlysosomal membrane\tP07355,Q9UM54,Q8IWA5,O75348,Q99519,O00203,P22314,Q96JC1,P51148,B7Z2Y1,P12109,P27487,Q9H270,P38606,P15144\nintracellular membrane-bounded organelle\tP35998,P04406,O14745,Q99519,P39656,Q96EY1,Q96S66,O14964,Q9NV92,P46379,O00429,O95070,Q9Y265,P05023,Q8N0Z8,P25205,P51148,Q9UDR5,Q6P2E9,O60762,Q9H814,P67809,O14980,Q15365,P53618\nmitochondrial outer membrane\tQ07817,Q16548,O94826,Q92843,O00429,Q86UT6,Q07820,Q99623,P10415,Q8N0X7\nintegral component of endoplasmic reticulum membrane\tQ9NZ01,O00124,Q14739,P11021,P57088,Q9P035,Q9NQC3,O76024\ndesmosome\tP15924,Q99569,P22314,Q9H307\ncentrosome\tQ6NXT6,O14745,Q5SW79,Q96CW5,Q8TF76,Q7Z4H7,P17987,Q13561,P06493,P35222,P25205,Q9NQG5,P0DP23,Q07817,Q9H814,Q9BSJ2,P48643,Q8IWR1,Q9P289,P12004\nfascia adherens\tP12814,P15924,P35222\ncytoplasmic ribonucleoprotein granule\tP11940,P62753,Q00839,P19338\ncaveola\tP22460,P37173,Q14524,P09601,P05023,P05026\nendoplasmic reticulum chaperone complex\tQ8IXB1,P11021,Q9Y4L1\nAP-3 adaptor complex\tQ9H270,O00203,Q96JC1\nDNA-directed RNA polymerase II,holoenzyme\tQ9NQG5,Q9UM54,O94762\nnuclear euchromatin\tP16402,P35222,P24928,P38159\nPcG protein complex\tP19784,Q9BSM1,P67870,P68400\nruffle\tP07355,P12814,Q5JWF2,Q9UM54,O14745,P05556,Q14155\nribosome\tP60866,P08708,Q86WX3,Q92552,P62269,P39019,P42677,P61927,P62753,Q9Y3U8\ntransport vesicle\tQ5JWF2,P53621,Q9UBF2,Q9H8W4,P35606,Q15818,P53618\ncell-cell junction\tP07355,Q9NYL9,P12814,Q99569,Q14160,Q9H307,O43707,P61158,P35222,Q12959\nRNA cap binding complex\tQ09161,Q53F19\nScrib-APC-beta-catenin complex\tQ14160,P35222\nproteasome storage granule\tQ13200,Q99460\n',
      },
    ],
  },
  gallus: {
    title: 'Gallus gallus (BioGRID)',
    source: 'BioGRID database',
    link: null,
    files: [
      {
        kind: 'network',
        name: 'Gallus gallus BioGRID',
        fileName: 'Biogrid_no_self_loops.txt',
        text: 'Source\tTarget\nP62760\tP60706\nP62764\tP60706\nO15392\tP53352\nQ9IAY5\tP49024\nQ9IAY5\tP49024\nP49024\tQ9IAY5\nQ13625\tP46936\nO93512\tO60542\nP41238\tQ7T2T1\nO42414\tQ9JI92\nO42414\tQ9JI92\nP05556\tQ00944\nP05556\tP49024\nP27986\tQ9DDT2\nP28497\tP23297\nP49024\tP12003\nP49024\tQ00944\nQ4KWZ7\tQ9DEA3\nQ9DEA3\tQ4KWZ7\nP53478\tQ9UUJ1\nP53478\tP32390\nP53478\tP78929\nP12003\tP60010\nP79987\tP56517\nP79987\tP56519\nP79987\tQ9W7I5\nP79987\tQ3C1E9\nQ9W7I5\tP79987\nP56517\tP79987\nP56519\tP79987\nQ5ZJY5\tO93257\nQ5R1T0\tP56517\nQ5R1T0\tP56519\nQ5R1T0\tP56517\nQ5R1T0\tP56519\nQ5R1T0\tP56520\nQ09472\tP17678\nP23204\tP17678\nP23204\tP23824\nP23204\tP23825\nQ92993\tP70082\nQ92993\tP0C1H3\nQ92993\tP84247\nQ92993\tP62801\nP56519\tQ9HAZ2\nO18738\tP31696\nQ8AYS7\tQ1T7B8\nQ8AYS7\tQ1T7C0\nQ8AYS7\tQ1T7B7\nQ8AYS7\tQ1T7C1\nQ8AYS7\tQ1T7B9\nQ90ZF9\tQ1T7B8\nQ90ZF9\tQ1T7C0\nQ90ZF9\tQ1T7B7\nQ90ZF9\tQ1T7C1\nQ90ZF9\tQ1T7B9\nQ76I90\tQ76I89\nQ76I89\tQ76I90\nQ8AYS8\tP09572\nQ8AYS8\tP11501\nQ8AYS8\tQ90593\nQ8AYS8\tP16053\nQ8AYS8\tP08106\nQ8AYS8\tO73885\nQ8AYS8\tQ5ZHY5\nQ8AYS8\tQ5ZL72\nQ8AYS8\tP09207\nQ8AYS8\tQ5ZLC5\nQ8AYS8\tP05094\nQ8AYS8\tP00508\nQ8AYS8\tP54097\nQ8AYS8\tP17153\nQ8AYS8\tQ5ZMT0\nQ8AYS8\tP28497\nQ8AYS8\tP62207\nQ8AYS8\tQ5ZKC9\nQ8AYS8\tQ5F3W6\nQ8AYS8\tP60878\nQ8AYS8\tQ5ZM44\nQ8AYS8\tP08250\nQ8AYS8\tQ5ZMB2\nQ8AYS8\tQ00649\nQ8AYS8\tQ5ZKK4\nQ8AYS8\tQ5ZIV5\nQ8AYS8\tQ5ZMD1\nQ8AYS8\tQ5ZLQ6\nQ8AYS8\tP07090\nQ8AYS8\tP81628\nQ8AYS8\tP42324\nQ8AYS8\tP62758\nQ8AYS8\tP62764\nQ8AYS8\tP08110\nQ8AYS8\tO93510\nQ8AYS8\tP13731\nQ8AYS8\tQ5F425\nQ8AYS8\tP18359\nQ8AYS8\tP02789\nQ8AYS8\tQ5ZL57\nQ8AYS8\tP19121\nQ8AYS8\tO57391\nQ8AYS8\tQ9PTG6\nQ8AYS8\tP63270\nQ8AYS8\tP51913\nQ8AYS8\tP05122\nQ8AYS8\tQ5ZME2\nQ8AYS8\tQ5ZHP5\nQ8AYS8\tP48463\nQ8AYS8\tP04354\nQ8AYS8\tP50890\nQ8AYS8\tP05419\nQ8AYS8\tP80566\nQ8AYS8\tP31395\nQ8AYS8\tP00940\nQ8AYS8\tQ5ZLN1\nQ8AYS8\tQ5ZJF4\nQ8AYS8\tP00340\nQ8AYS8\tP09654\nQ8AYS8\tO42163\nQ8AYS8\tP16580\nQ8AYS8\tP02112\nQ8AYS8\tP80026\nQ8AYS8\tQ5ZLG1\nQ8AYS8\tO57535\nQ8AYS8\tQ07212\nQ8AYS8\tO13268\nQ8AYS8\tP00337\nQ8AYS8\tP00356\nQ8AYS8\tP51903\nQ8AYS8\tP07341\nQ8AYS8\tP00548\nQ8AYS8\tP14732\nQ8AYS8\tP13648\nQ8AYS8\tQ5F3W6\nQ8AYS8\tP42324\nQ8AYS8\tP60878\nQ8AYS8\tP17153\nQ8AYS8\tQ5ZL72\nQ8AYS8\tQ5F425\nQ8AYS8\tP80566\nQ8AYS8\tQ01406\nQ8AYS8\tP08106\nQ8AYS8\tP31395\nQ8AYS8\tQ90593\nQ8AYS8\tQ04619\nQ8AYS8\tP14731\nQ5F3W6\tQ8AYS8\nP42324\tQ8AYS8\nP60878\tQ8AYS8\nP17153\tQ8AYS8\nQ5ZL72\tQ8AYS8\nQ5F425\tQ8AYS8\nP80566\tQ8AYS8\nQ01406\tQ8AYS8\nP08106\tQ8AYS8\nP31395\tQ8AYS8\nQ90593\tQ8AYS8\nQ04619\tQ8AYS8\nP14731\tQ8AYS8\nP84023\tP49140\nP84023\tP83038\nP38531\tP04637\nP38531\tP06876\nP38531\tP01103\nF1N9Y5\tP00548\nF1N9Y5\tP09244\nF1N9Y5\tO73885\nF1N9Y5\tQ5ZL72\nF1N9Y5\tP11501\nF1N9Y5\tQ90705\nF1N9Y5\tQ5ZLC5\nF1N9Y5\tP51913\nF1N9Y5\tQ5ZM98\nF1N9Y5\tP38024\nF1N9Y5\tP47826\nF1N9Y5\tP08070\nF1N9Y5\tQ90593\nF1N9Y5\tP15771\nF1N9Y5\tQ9IAY5\nF1N9Y5\tP50890\nF1N9Y5\tQ5ZJK8\nF1N9Y5\tP51903\nF1N9Y5\tP67883\nF1N9Y5\tP00340\nF1N9Y5\tQ6EE31\nF1N9Y5\tP22451\nF1N9Y5\tQ8UVD9\nF1N9Y5\tQ5ZJU3\nF1N9Y5\tO57535\nF1N9Y5\tQ5ZLP8\nF1N9Y5\tQ04619\nF1N9Y5\tQ5ZIQ3\nF1N9Y5\tP47836\nF1N9Y5\tQ5ZKC9\nF1N9Y5\tQ5ZK03\nF1N9Y5\tQ8AYC9\nF1N9Y5\tP00356\nF1N9Y5\tQ5ZLN4\nF1N9Y5\tQ5ZJ56\nF1N9Y5\tP18660\nF1N9Y5\tQ5ZMT0\nF1N9Y5\tQ8JFP1\nF1N9Y5\tP32429\nF1N9Y5\tP16039\nF1N9Y5\tP00337\nF1N9Y5\tP08106\nF1N9Y5\tQ5ZMS3\nF1N9Y5\tP61355\nF1N9Y5\tQ5ZJ54\nF1N9Y5\tP63247\nF1N9Y5\tQ5ZKA5\nF1N9Y5\tP08629\nF1N9Y5\tP16527\nF1N9Y5\tQ5ZMN2\nF1N9Y5\tQ5ZJZ5\nF1N9Y5\tP18359\nF1N9Y5\tQ5ZKF5\nF1N9Y5\tQ5ZLN1\nF1N9Y5\tQ9YGC1\nF1N9Y5\tQ5ZLE6\nF1N9Y5\tQ8UW59\nF1N9Y5\tQ5ZKB9\nF1N9Y5\tP13863\nF1N9Y5\tQ90WU3\nF1N9Y5\tQ5ZI72\nF1N9Y5\tQ5ZIA5\nF1N9Y5\tP11029\nF1N9Y5\tQ98TF8\nF1N9Y5\tQ5ZL42\nF1N9Y5\tQ5ZKC1\nF1N9Y5\tP17790\nF1N9Y5\tQ5ZMN3\nF1N9Y5\tQ98TF6\nF1N9Y5\tQ5ZLC6\nF1N9Y5\tP24367\nF1N9Y5\tP11799\nF1N9Y5\tQ5ZK62\nF1N9Y5\tP26584\nF1N9Y5\tP14315\nF1N9Y5\tQ5ZKG5\nF1N9Y5\tP42558\nF1N9Y5\tP41239\nF1N9Y5\tP05419\nF1N9Y5\tP43347\nF1N9Y5\tP63283\nF1N9Y5\tQ5ZJN4\nF1N9Y5\tQ5ZL57\nF1N9Y5\tQ8JG64\nF1N9Y5\tO42392\nF1N9Y5\tQ5ZK01\nF1N9Y5\tP19179\nF1N9Y5\tO93256\nF1N9Y5\tQ02391\nP28683\tP62140\nQ6A078\tP00698\nP68400\tO13016\nP12931\tO13016\nP07812\tQ04619\nP07812\tP08106\nP62207\tQ90623\nQ90ZF9\tQ76I89\nQ90ZF9\tQ76I90\nQ90ZF9\tQ76I89\nP00523\tP19785\nP11501\tQ5ZL72\nP11501\tP08106\nQ04619\tQ5ZL72\nQ04619\tP08106\nP68399\tP01109\nP67868\tP01109\nQ12341\tP02263\nQ12341\tP62801\nQ5ZL72\tP08106\nQ5ZL72\tQ04619\nQ04619\tP08106\nQ06592\tP62801\nQ06592\tP84247\nP39979\tP62801\nQ3C1E9\tP79987\nQ3C1E9\tQ5R1S9\nQ3C1E9\tP84247\nQ3C1E9\tP62801\nP07812\tQ15185\nP07812\tP07900\nP07812\tP34932\nP07812\tP25685\nP07812\tP31948\nP25293\tP70082\nP25293\tP0C1H3\nP25293\tP84247\nP25293\tP62801\nQ702N8\tP68139\nA4UGR9\tP68139\nP68139\tP42639\nP42684\tQ04929\nQ92831\tP02263\nQ92831\tP62801\nQ9DEA3\tQ4KWZ7\nQ02842\tQ5ZJL9\nQ15185\tP11501\nP00523\tP51913\nQ04982\tQ5ZIK9\nQ04982\tP48463\nQ04982\tQ5ZM98\nQ04982\tP67869\nQ04982\tO73885\nQ04982\tQ5ZMN3\nQ04982\tP16039\nQ04982\tP42558\nQ04982\tQ6U7I1\nQ04982\tP05625\nQ04982\tQ5ZMD1\nQ04982\tQ5ZLQ6\nQ04982\tQ5ZMT0\nQ04982\tQ5ZKC9\nQ04982\tP50147\nQ04982\tQ5F3W6\nQ04982\tO57476\nQ04982\tP11501\nQ04982\tQ90828\nQ04982\tQ90891\nQ04982\tQ90593\nQ04982\tP08110\nP18652\tO95071\nO42414\tP16092\nP16092\tO42414\nP61088\tQ9DEA3\nQ15819\tQ9DEA3\nQ9W7C5\tQ401C0\nQ401C0\tQ9W7C5\nP11275\tP13387\nQ7T0L4\tQ9PU53\nQ07496\tP32004\n',
      },
      {
        kind: 'annotation',
        name: 'Gallus gallus KEGG pathways',
        fileName: 'BioGrid_Chicken_Gallus_Pathways_KEGG_PATHWAY_FILTERED.txt',
        text: 'ErbB signaling pathway\tQ00944,Q04929,P00523,P01109,Q90891,P13387,Q9W7C5,P05625,Q04982\nCarbon metabolism\tP00356,O57391,P00548,P07341,P51903,Q5ZLN1,Q5ZME2,P51913,P00940,P00508\nBiosynthesis of antibiotics\tP00356,O57391,P00548,P07341,O57535,P51903,Q5ZLN1,P00337,Q5ZME2,P51913,P00940,P00508,P00340,P38024\nGap junction\tP00523,P09244,Q90891,P08070,P13387,P50147,P09207,P05625,P13863\nRibosome\tQ98TF6,Q98TF8,P67883,P32429,P61355,P47826,Q5ZJ56,P47836,P50890,P18660,P22451\nFocal adhesion\tQ00944,Q04929,P00523,Q90623,P13387,P05094,P60706,P53478,P11799,P49024,P12003,Q04982,P62207,P05625\nProgesterone-mediated oocyte maturation\tQ04619,P07812,P18652,P11501,P50147,P05625,Q04982,P13863\nVascular smooth muscle contraction\tQ8AYS8,P11799,Q90623,P05419,P63270,Q90891,P62207,P05625,Q04982\nProtein processing in endoplasmic reticulum\tQ04619,Q5ZKF5,Q90593,Q8JG64,O73885,P11501,P08110,Q5ZHY5,P81628,Q5ZK03,P08106\nPyruvate metabolism\tP00548,P00337,Q5ZME2,P11029,P00340\nVEGF signaling pathway\tQ00944,P00523,P49024,Q90891,P05625,Q00649\nInsulin signaling pathway\tQ04929,P05419,Q90891,P11029,P62207,P05625,Q04982,O13016\nTight junction\tP00523,P48463,P05094,P60706,P53478,Q01406',
      },
    ],
  },
  'docs-tp53': {
    title: 'TP53 (NORMA documentation)',
    source: 'Example from the NORMA help pages',
    link: null,
    files: [
      {
        kind: 'network',
        name: 'TP53 docs network',
        fileName: '',
        text: 'Source\tTarget\tWeight\nCDKN1A\tTP53\t5\nTP53\tMDM2\t1\nMDM4\tTP53\t3\nBCL2L1\tTP53\t4\nCHEK2\tATM\t2\nTP53\tEP300\t1\nATM\tTP53\t4\nTP53\tCREBBP\t1\nMDM4\tMDM2\t1\nCHEK2\tTP53\t2\nTP53BP2\tTP53\t8\nCDKN2A\tTP53\t3\nCDKN2A\tMDM2\t3\nATM\tMDM2\t1\nEP300\tCREBBP\t2\n',
      },
      {
        kind: 'annotation',
        name: 'TP53 docs groups',
        fileName: '',
        text: 'Group-2\tBCL2L1,MDM4,MDM2,CHEK2\nGroup-5\tTP53,EP300\nGroup-1\tCDKN2A,ATM,TP53BP2,MDM2\nGroup-4\tCHEK2,CREBBP,MDM2\nGroup-3\tTP53,BCL2L1\nGroup-6\tMDM4,MDM2\n',
      },
      {
        kind: 'colors',
        name: 'TP53 docs expression',
        fileName: '',
        text: 'CDKN1A\tblue\nTP53\tblue\nMDM4\t#00ff00\nBCL2L1\tred\nCHEK2\tred\nATM\tred\nTP53BP2\tred\nCDKN2A\tblue\nEP300\t#ffff00\nCREBBP\tred\nMDM2\tblue\n',
      },
    ],
  },
}

/* ============================================================
   TABS AND HELP LINKS
   ============================================================ */
export const TAB_VIEWS = {
  welcome: 'welcomeView',
  network: 'canvas',
  network3d: 'canvas',
  profiler: 'profilerView',
  compare: 'compareView',
  help: 'helpView',
  about: 'aboutView',
  api: 'apiView',
}

export let currentTab = 'welcome'

export function switchTab(name, helpAnchor) {
  if (!TAB_VIEWS[name]) return
  currentTab = name
  document.querySelectorAll('#tabs .tab').forEach((btn) => {
    const on = btn.dataset.tab === name
    btn.setAttribute('aria-selected', on ? 'true' : 'false')
    btn.tabIndex = on ? 0 : -1
  })
  const onNetwork = name === 'network' || name === 'network3d'
  document.getElementById('canvas').classList.toggle('inactive', !onNetwork)
  if (typeof setMode3d === 'function' && (name === 'network3d') !== net3d.active)
    setMode3d(name === 'network3d')
  document.getElementById('welcomeView').classList.toggle('active', name === 'welcome')
  document.getElementById('profilerView').classList.toggle('active', name === 'profiler')
  if (name === 'profiler' && typeof refreshEnrichmentChoices === 'function')
    refreshEnrichmentChoices()
  document.getElementById('compareView').classList.toggle('active', name === 'compare')
  document.getElementById('helpView').classList.toggle('active', name === 'help')
  document.getElementById('aboutView').classList.toggle('active', name === 'about')
  document.getElementById('apiView').classList.toggle('active', name === 'api')
  if (name === 'network') {
    document.getElementById('cy').style.visibility = ''
    cy.resize()
    resizeHullCanvas()
    drawGroupHulls()
  }
  if (name === 'profiler') renderProfilerNetworkList()
  if (name === 'compare') renderCompareList()
  if ((name === 'help' || name === 'about' || name === 'api') && helpAnchor) {
    const target = document.getElementById(helpAnchor)
    if (target) {
      target.scrollIntoView({ block: 'start' })
      target.classList.remove('flash')
      void target.offsetWidth
      target.classList.add('flash')
    }
  }
}

/* ============================================================
   NETWORK PROFILER
   Topology statistics following igraph's definitions, computed on a
   simple undirected graph (self-loops and parallel edges set aside).
   Path-based measures use one BFS per node (Brandes' algorithm for
   betweenness), so they are skipped above PROFILE_PATH_LIMIT nodes.
   ============================================================ */
export const PROFILE_PATH_LIMIT = 8000

export const PROFILE_STATS = [
  { key: 'n', label: 'Nodes', igraph: 'vcount(g)', desc: 'Number of nodes.' },
  {
    key: 'm',
    label: 'Edges',
    igraph: 'ecount(g)',
    desc: 'Connections between distinct node pairs.',
  },
  {
    key: 'density',
    label: 'Density',
    igraph: 'edge_density(g)',
    desc: 'Share of all possible node pairs that are connected.',
  },
  {
    key: 'avgDegree',
    label: 'Average degree',
    igraph: 'mean(degree(g))',
    desc: 'Mean number of neighbors per node.',
  },
  {
    key: 'maxDegree',
    label: 'Maximum degree',
    igraph: 'max(degree(g))',
    desc: 'Most neighbors of any node.',
  },
  {
    key: 'isolated',
    label: 'Isolated nodes',
    igraph: 'sum(degree(g) == 0)',
    desc: 'Nodes without any connection.',
  },
  {
    key: 'components',
    label: 'Connected components',
    igraph: 'count_components(g)',
    desc: 'Separate pieces of the network.',
  },
  {
    key: 'largest',
    label: 'Largest component',
    igraph: 'max(components(g)$csize)',
    desc: 'Nodes in the biggest piece, with its share of all nodes.',
  },
  {
    key: 'diameter',
    label: 'Diameter',
    igraph: 'diameter(g)',
    desc: 'Longest shortest path between any two connected nodes.',
  },
  {
    key: 'radius',
    label: 'Radius',
    igraph: 'radius(g)',
    desc: 'Smallest eccentricity of any node.',
  },
  {
    key: 'avgPath',
    label: 'Average path length',
    igraph: 'mean_distance(g)',
    desc: 'Mean shortest-path length over all connected node pairs.',
  },
  {
    key: 'avgEcc',
    label: 'Average eccentricity',
    igraph: 'mean(eccentricity(g))',
    desc: 'Mean, over nodes, of the distance to the farthest reachable node.',
  },
  {
    key: 'transitivity',
    label: 'Clustering coefficient',
    igraph: 'transitivity(g, "global")',
    desc: 'Share of connected triples that close into triangles.',
  },
  {
    key: 'avgLocal',
    label: 'Average local clustering',
    igraph: 'transitivity(g, "average")',
    desc: "Mean of each node's own clustering coefficient, over nodes with two or more neighbors.",
  },
  {
    key: 'triangles',
    label: 'Triangles',
    igraph: 'sum(count_triangles(g)) / 3',
    desc: 'Number of three-node cycles.',
  },
  {
    key: 'assortativity',
    label: 'Degree assortativity',
    igraph: 'assortativity_degree(g)',
    desc: 'Above 0: hubs link to hubs. Below 0: hubs link to sparsely connected nodes.',
  },
  {
    key: 'centrDegree',
    label: 'Degree centralization',
    igraph: 'centr_degree(g, loops = FALSE)$centralization',
    desc: 'How strongly connections concentrate on one node (1 for a star).',
  },
  {
    key: 'centrBetw',
    label: 'Betweenness centralization',
    igraph: 'centr_betw(g)$centralization',
    desc: 'How strongly shortest paths run through one node.',
  },
  {
    key: 'centrClo',
    label: 'Closeness centralization',
    igraph: 'centr_clo(g)$centralization',
    desc: 'How much one node is closer to all others than the rest are.',
  },
  {
    key: 'modularity',
    label: 'Modularity (Louvain)',
    igraph: 'modularity(cluster_louvain(g))',
    desc: 'How well the network splits into dense communities; above about 0.3 suggests clear structure.',
  },
  {
    key: 'communities',
    label: 'Communities (Louvain)',
    igraph: 'length(cluster_louvain(g))',
    desc: 'Number of communities Louvain finds, including single nodes.',
  },
  {
    key: 'setAside',
    label: 'Set aside before profiling',
    igraph: 'simplify(g)',
    desc: 'Self-loops and parallel edges not counted as connections.',
  },
]

export function formatStat(v) {
  if (v === null || v === undefined || (typeof v === 'number' && Number.isNaN(v))) return '—'
  if (typeof v === 'string') return v
  if (!Number.isFinite(v)) return '—'
  if (Number.isInteger(v)) return v.toLocaleString('en-US')
  const abs = Math.abs(v)
  if (abs !== 0 && abs < 0.001) return v.toExponential(2)
  return (+v.toFixed(4)).toString()
}

// ids: node names; pairs: [source, target] per edge
export function simpleGraph(ids, pairs) {
  const index = new Map(ids.map((id, i) => [id, i]))
  const n = ids.length
  const sets = Array.from({ length: n }, () => new Set())
  let loops = 0,
    parallel = 0
  pairs.forEach(([s, t]) => {
    const a = index.get(s),
      b = index.get(t)
    if (a === undefined || b === undefined) return
    if (a === b) {
      loops++
      return
    }
    if (sets[a].has(b)) {
      parallel++
      return
    }
    sets[a].add(b)
    sets[b].add(a)
  })
  const adj = sets.map((x) => Int32Array.from(x))
  let m = 0
  adj.forEach((a) => {
    m += a.length
  })
  return { ids, n, m: m / 2, adj, loops, parallel }
}

export function profileGraph(g) {
  const { n, m, adj } = g
  const deg = adj.map((a) => a.length)
  const r = { n, m, degree: deg }
  r.density = n > 1 ? (2 * m) / (n * (n - 1)) : NaN
  r.avgDegree = n ? (2 * m) / n : NaN
  r.maxDegree = n ? Math.max(...deg) : NaN
  r.isolated = deg.filter((d) => d === 0).length
  r.setAside =
    g.loops || g.parallel
      ? [
          g.loops ? plural(g.loops, 'self-loop') : '',
          g.parallel ? plural(g.parallel, 'parallel edge') : '',
        ]
          .filter(Boolean)
          .join(', ')
      : 'none'

  // connected components
  const comp = new Int32Array(n).fill(-1)
  const queue = new Int32Array(Math.max(1, n))
  const compSizes = []
  for (let s = 0; s < n; s++) {
    if (comp[s] >= 0) continue
    const c = compSizes.length
    let head = 0,
      tail = 0
    queue[tail++] = s
    comp[s] = c
    while (head < tail) {
      const v = queue[head++]
      const nb = adj[v]
      for (let k = 0; k < nb.length; k++) {
        const w = nb[k]
        if (comp[w] < 0) {
          comp[w] = c
          queue[tail++] = w
        }
      }
    }
    compSizes.push(tail)
  }
  r.components = compSizes.length
  const largest = compSizes.length ? Math.max(...compSizes) : 0
  r.largest = n ? `${largest.toLocaleString('en-US')} (${((100 * largest) / n).toFixed(1)}%)` : '—'

  // triangles and clustering
  const tri = new Float64Array(n)
  const mark = new Int32Array(n).fill(-1)
  for (let u = 0; u < n; u++) {
    const nu = adj[u]
    for (let k = 0; k < nu.length; k++) mark[nu[k]] = u
    for (let k = 0; k < nu.length; k++) {
      const v = nu[k]
      if (v <= u) continue
      const nv = adj[v]
      for (let q = 0; q < nv.length; q++) {
        const w = nv[q]
        if (w > v && mark[w] === u) {
          tri[u]++
          tri[v]++
          tri[w]++
        }
      }
    }
  }
  let triSum = 0,
    triples = 0,
    localSum = 0,
    localCount = 0
  const localClustering = new Float64Array(n)
  for (let v = 0; v < n; v++) {
    const d = deg[v]
    triSum += tri[v]
    const t = (d * (d - 1)) / 2
    triples += t
    if (d >= 2) {
      localClustering[v] = tri[v] / t
      localSum += localClustering[v]
      localCount++
    }
  }
  r.triangles = triSum / 3
  r.transitivity = triples > 0 ? triSum / triples : NaN
  r.avgLocal = localCount ? localSum / localCount : NaN
  r.localClustering = localClustering

  // degree assortativity (Newman), over each undirected edge once
  if (m > 0) {
    let sProd = 0,
      sMean = 0,
      sSq = 0
    for (let u = 0; u < n; u++) {
      const nu = adj[u]
      for (let k = 0; k < nu.length; k++) {
        const v = nu[k]
        if (v <= u) continue
        const j = deg[u],
          q = deg[v]
        sProd += j * q
        sMean += (j + q) / 2
        sSq += (j * j + q * q) / 2
      }
    }
    const mean = sMean / m
    const denom = sSq / m - mean * mean
    r.assortativity = Math.abs(denom) > 1e-12 ? (sProd / m - mean * mean) / denom : NaN
  } else {
    r.assortativity = NaN
  }

  r.centrDegree = n > 2 ? deg.reduce((s, d) => s + (r.maxDegree - d), 0) / ((n - 1) * (n - 2)) : NaN

  // shortest paths, eccentricity, closeness, betweenness
  if (n <= PROFILE_PATH_LIMIT) {
    const dist = new Int32Array(n).fill(-1)
    const sigma = new Float64Array(n)
    const delta = new Float64Array(n)
    const order = new Int32Array(Math.max(1, n))
    const betw = new Float64Array(n)
    const ecc = new Float64Array(n)
    const closeness = new Float64Array(n).fill(NaN)
    let pathSum = 0,
      pathCount = 0,
      diameter = 0
    for (let s = 0; s < n; s++) {
      let head = 0,
        tail = 0
      order[tail++] = s
      dist[s] = 0
      sigma[s] = 1
      let far = 0,
        sum = 0
      while (head < tail) {
        const v = order[head++]
        const dv = dist[v]
        if (dv > far) far = dv
        sum += dv
        const nb = adj[v]
        for (let k = 0; k < nb.length; k++) {
          const w = nb[k]
          if (dist[w] < 0) {
            dist[w] = dv + 1
            order[tail++] = w
          }
          if (dist[w] === dv + 1) sigma[w] += sigma[v]
        }
      }
      const reached = tail - 1
      ecc[s] = far
      if (far > diameter) diameter = far
      if (reached > 0) {
        closeness[s] = reached / sum
        pathSum += sum
        pathCount += reached
      }
      for (let i = tail - 1; i >= 0; i--) {
        const w = order[i]
        const nb = adj[w]
        for (let k = 0; k < nb.length; k++) {
          const v = nb[k]
          if (dist[v] === dist[w] - 1) delta[v] += (sigma[v] / sigma[w]) * (1 + delta[w])
        }
        if (w !== s) betw[w] += delta[w]
      }
      for (let i = 0; i < tail; i++) {
        const v = order[i]
        dist[v] = -1
        sigma[v] = 0
        delta[v] = 0
      }
    }
    for (let v = 0; v < n; v++) betw[v] /= 2
    r.betweenness = betw
    r.closeness = closeness
    r.diameter = n ? diameter : NaN
    r.radius = n ? Math.min(...ecc) : NaN
    r.avgEcc = n ? ecc.reduce((a, b) => a + b, 0) / n : NaN
    r.avgPath = pathCount ? pathSum / pathCount : NaN
    const maxB = n ? Math.max(...betw) : 0
    r.centrBetw =
      n > 2 ? betw.reduce((s, b) => s + (maxB - b), 0) / (((n - 1) * (n - 1) * (n - 2)) / 2) : NaN
    const finiteClo = [...closeness].filter(Number.isFinite)
    const maxC = finiteClo.length ? Math.max(...finiteClo) : NaN
    r.centrClo =
      n > 2 && finiteClo.length
        ? finiteClo.reduce((s, c) => s + (maxC - c), 0) / (((n - 1) * (n - 2)) / (2 * n - 3))
        : NaN
  } else {
    r.pathsSkipped = true
    ;['diameter', 'radius', 'avgEcc', 'avgPath', 'centrBetw', 'centrClo'].forEach((k) => {
      r[k] = NaN
    })
  }

  const lv = louvain(g)
  r.louvain = lv
  r.modularity = lv.modularity
  r.communities = lv.count
  return r
}

// Louvain community detection (Blondel et al. 2008), unweighted input,
// nodes visited in a fixed order so results are repeatable.
export function louvain(g, gamma = 1) {
  const n = g.n
  const twoM = 2 * g.m
  if (!twoM) {
    return {
      membership: Int32Array.from({ length: n }, (_, i) => i),
      count: n,
      modularity: NaN,
      sizes: new Array(n).fill(1),
    }
  }
  let nbr = g.adj.map((a) => {
    const mp = new Map()
    a.forEach((v) => mp.set(v, 1))
    return mp
  })
  let size = n
  let membership = Int32Array.from({ length: n }, (_, i) => i)
  for (let level = 0; level < 32; level++) {
    const k = new Float64Array(size)
    nbr.forEach((mp, i) => {
      let s = 0
      mp.forEach((w) => {
        s += w
      })
      k[i] = s
    })
    const comm = Int32Array.from({ length: size }, (_, i) => i)
    const tot = Float64Array.from(k)
    let moved = true,
      anyMove = false,
      passes = 0
    while (moved && passes < 100) {
      moved = false
      passes++
      for (let i = 0; i < size; i++) {
        const ci = comm[i],
          ki = k[i]
        const links = new Map()
        nbr[i].forEach((w, j) => {
          if (j !== i) {
            const cj = comm[j]
            links.set(cj, (links.get(cj) || 0) + w)
          }
        })
        tot[ci] -= ki
        let best = ci
        let bestGain = (links.get(ci) || 0) - (gamma * tot[ci] * ki) / twoM
        links.forEach((w, c) => {
          const gain = w - (gamma * tot[c] * ki) / twoM
          if (gain > bestGain + 1e-12) {
            bestGain = gain
            best = c
          }
        })
        tot[best] += ki
        if (best !== ci) {
          comm[i] = best
          moved = true
          anyMove = true
        }
      }
    }
    if (!anyMove) break
    const remap = new Map()
    for (let i = 0; i < size; i++) if (!remap.has(comm[i])) remap.set(comm[i], remap.size)
    const next = remap.size
    const agg = Array.from({ length: next }, () => new Map())
    for (let i = 0; i < size; i++) {
      const ci = remap.get(comm[i])
      nbr[i].forEach((w, j) => {
        const cj = remap.get(comm[j])
        agg[ci].set(cj, (agg[ci].get(cj) || 0) + w)
      })
    }
    membership = membership.map((x) => remap.get(comm[x]))
    nbr = agg
    if (next === size) break
    size = next
  }
  // modularity on the original graph
  let count = 0
  membership.forEach((c) => {
    if (c + 1 > count) count = c + 1
  })
  const inW = new Float64Array(count),
    totW = new Float64Array(count),
    sizes = new Array(count).fill(0)
  for (let u = 0; u < n; u++) {
    const cu = membership[u]
    sizes[cu]++
    totW[cu] += g.adj[u].length
    g.adj[u].forEach((v) => {
      if (membership[v] === cu) inW[cu] += 1
    })
  }
  let q = 0
  for (let c = 0; c < count; c++) q += inW[c] / twoM - Math.pow(totW[c] / twoM, 2)
  return { membership, count, modularity: q, sizes }
}

// page wiring, run by main.ts in the original order
export function init() {
  /* ---------- wiring ---------- */
  document
    .getElementById('btnNormaAdd')
    .addEventListener('click', () => document.getElementById('normaFileInput').click())

  document.getElementById('normaFileInput').addEventListener('change', (e) => {
    handleNormaUploads(e.target.files)
    e.target.value = ''
  })

  ;(function setupDropZone() {
    const zone = document.getElementById('dropZone')
    let depth = 0
    zone.addEventListener('dragenter', (e) => {
      e.preventDefault()
      depth++
      zone.classList.add('dragging')
    })
    zone.addEventListener('dragover', (e) => {
      e.preventDefault()
      e.dataTransfer.dropEffect = 'copy'
    })
    zone.addEventListener('dragleave', () => {
      depth = Math.max(0, depth - 1)
      if (!depth) zone.classList.remove('dragging')
    })
    zone.addEventListener('drop', (e) => {
      e.preventDefault()
      depth = 0
      zone.classList.remove('dragging')
      if (e.dataTransfer.files && e.dataTransfer.files.length)
        handleNormaUploads(e.dataTransfer.files)
    })
  })()

  document.getElementById('btnRefreshView').addEventListener('click', () => {
    const v = activeView()
    refreshLibraryView()
    if (v && v.autoName && cy.nodes().length) {
      v.name = uniqueViewName(nameForSelection())
      renderViewBar()
    }
  })

  document.getElementById('btnOpenNewView').addEventListener('click', () => {
    if (!libSelection.networks.size) {
      setStatus('normaStatus', [{ level: 'error', text: 'Tick at least one network first.' }])
      return
    }
    // the ticked files move to the new view; this view keeps what it shows
    const pending = {
      networks: [...libSelection.networks],
      annotation: libSelection.annotation,
      colors: libSelection.colors,
    }
    const cur = activeView()
    const shown = selectionFromKey(S.currentLibView)
    const name = nameForSelection()
    if (cur && cy.nodes().length) {
      libSelection.networks = new Set(shown.networks)
      libSelection.annotation = shown.annotation
      libSelection.colors = shown.colors
    }
    openInNewView(name, () => {
      libSelection.networks = new Set(pending.networks)
      libSelection.annotation = pending.annotation
      libSelection.colors = pending.colors
      refreshLibraryView()
    })
  })

  document.getElementById('btnNormaExportNetwork').addEventListener('click', exportNormaNetwork)

  document
    .getElementById('btnNormaExportAnnotation')
    .addEventListener('click', exportNormaAnnotation)

  document.getElementById('btnNormaExportColors').addEventListener('click', exportNormaColors)

  // A numeric expression file for the documentation example (simulated values,
  // for trying the color scale; not measured data).
  NORMA_EXAMPLE_SETS['docs-tp53'].files.push({
    kind: 'colors',
    name: 'TP53 docs log2FC and adjusted p-values (simulated)',
    fileName: 'TP53_docs_values_simulated.txt',
    text: 'Gene\tlog2FC\tpadj\nCDKN1A\t2.4\t0.000001\nTP53\t-0.3\t0.41\nMDM4\t1.1\t0.02\nBCL2L1\t-1.8\t0.0003\nCHEK2\t-1.2\t0.004\nATM\t-0.9\t0.03\nTP53BP2\t-2.2\t0.00005\nCDKN2A\t1.7\t0.0002\nEP300\t0.4\t0.21\nCREBBP\t-0.6\t0.09\nMDM2\t2.9\t0.00000001\n',
  })

  // ---------- NORMA 3.0 examples that showcase the new features ----------
  ;(function addNorma3Examples() {
    // 1. The example network from Arena3D's documentation: layers become groups
    const arenaRows = [
      ['An', 'Group1', 'Cn', 'Group1', 2],
      ['An', 'Group1', 'Bn', 'Group1', 10],
      ['Bn', 'Group1', 'Cn', 'Group1', 1],
      ['Cn', 'Group1', 'Dn', 'Group1', 3],
      ['En', 'Group2', 'Fn', 'Group2', 4],
      ['En', 'Group2', 'Hn', 'Group2', 5],
      ['Fn', 'Group2', 'Gn', 'Group2', 6],
      ['Gn', 'Group2', 'Hn', 'Group2', 7],
      ['In', 'Group3', 'Jn', 'Group3', 8],
      ['Bn', 'Group1', 'Fn', 'Group2', 9],
      ['Dn', 'Group1', 'Hn', 'Group2', 11],
      ['Dn', 'Group1', 'In', 'Group3', 1],
      ['Cn', 'Group1', 'Jn', 'Group3', 1],
      ['Hn', 'Group2', 'In', 'Group3', 1],
      ['Hn', 'Group2', 'Kn', 'Group4', 1],
      ['Kn', 'Group4', 'Ln', 'Group4', 0.1],
      ['Kn', 'Group4', 'Mn', 'Group5', 1],
      ['An', 'Group1', 'Nn', 'Group5', 1],
      ['Kn', 'Group4', 'On', 'Group5', 1],
      ['Kn', 'Group4', 'Pn', 'Group5', 12],
      ['Kn', 'Group4', 'Qn', 'Group6', 1],
      ['Kn', 'Group4', 'Rn', 'Group6', 1],
      ['Kn', 'Group4', 'Sn', 'Group7', 1],
      ['Kn', 'Group4', 'Tn', 'Group7', 10],
    ]
    const arenaText =
      [
        'SourceNode\tSourceLayer\tTargetNode\tTargetLayer\tWeight\tChannel',
        ...arenaRows.map((r) => [...r, 1].join('\t')),
      ].join('\n') + '\n'
    const arenaConv = convertArena3dNetwork(arenaText)
    NORMA_EXAMPLE_SETS['arena3d'] = {
      title: 'Arena3D example: 7 layers',
      source: 'Arena3D documentation',
      link: 'https://arena3d.org',
      local: true,
      files: [
        {
          kind: 'network',
          name: 'Arena3D example network',
          fileName: 'arena3d_example_network.txt',
          text: arenaConv.network,
        },
        {
          kind: 'annotation',
          name: 'Arena3D example layers',
          fileName: 'arena3d_example_layers.txt',
          text: arenaConv.annotation,
        },
        {
          kind: 'raw',
          name: 'Arena3D network file (original format)',
          fileName: 'arena3d_example_upload_network.txt',
          text: arenaText,
        },
      ],
    }

    // 2. A directed signalling cascade with simulated fold changes
    const cascade = generateDirectedMultiNetwork(40)
    const cTexts = demoToNormaTexts(cascade)
    const rc = mulberry32(2024)
    const gauss = () => {
      let u = 0,
        v = 0
      while (!u) u = rc()
      while (!v) v = rc()
      return Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v)
    }
    const layerShift = { receptors: 1.6, kinases: 1.0, 'transcription factors': 0.3, targets: -1.4 }
    const cValues =
      [
        'Gene\tlog2FC\tpadj',
        ...cascade.nodes.map((n) => {
          if (rc() < 0.06) return `${n.id}\tNA\tNA`
          const fc = Math.round((layerShift[n.groups[0]] + gauss() * 0.9) * 1000) / 1000
          const p = Math.min(1, Math.pow(10, -Math.abs(fc) * (1.2 + rc() * 1.8)))
          return `${n.id}\t${fc}\t${p.toPrecision(3)}`
        }),
      ].join('\n') + '\n'
    NORMA_EXAMPLE_SETS['cascade'] = {
      title: 'Signalling cascade with fold changes (simulated)',
      source: 'Simulated for NORMA 3.0',
      link: null,
      local: true,
      display: {
        edgeDirection: 'data',
        edgeCurveStyle: 'bezier',
        layoutSelect: 'fr',
        legendShow: true,
        sizeMetric: 'fixed',
      },
      files: [
        {
          kind: 'network',
          name: 'Signalling cascade (directed, multi-edge)',
          fileName: 'cascade_network.txt',
          text: cTexts.network,
        },
        {
          kind: 'annotation',
          name: 'Cascade levels',
          fileName: 'cascade_levels.txt',
          text: cTexts.annotation,
        },
        {
          kind: 'colors',
          name: 'Cascade log2FC and padj (simulated)',
          fileName: 'cascade_values_simulated.txt',
          text: cValues,
        },
      ],
    }

    // 3. A healthy and a disease network (simulated) for Network Comparison
    const base = buildModuleNetwork({
      sizes: [18, 16, 16, 14],
      overlaps: [
        [0, 1],
        [1, 2],
        [2, 3],
        [3, 0],
        [1, 2, 3],
      ],
      pIn: 0.22,
      pOut: 0.01,
      seed: 64,
    })
    const rh = mulberry32(64)
    const ids = base.nodes.map((n) => n.id)
    const moduleOf = new Map(base.nodes.map((n) => [n.id, n.groups]))
    const healthy = base.edges.map((e) => [e.source, e.target])
    const key = (a, b) => (a < b ? a + '\t' + b : b + '\t' + a)
    const hSet = new Set(healthy.map(([a, b]) => key(a, b)))
    // disease: 20% of the edges are lost, and the third module gains new links
    const disease = healthy.filter(() => rh() > 0.2)
    const dSet = new Set(disease.map(([a, b]) => key(a, b)))
    const third = base.groupOrder
      ? base.groupOrder[2]
      : [...new Set(base.nodes.flatMap((n) => n.groups))][2]
    const inThird = ids.filter((id) => (moduleOf.get(id) || []).includes(third))
    let added = 0
    while (added < Math.round(healthy.length * 0.15)) {
      const a =
        rh() < 0.7 ? inThird[Math.floor(rh() * inThird.length)] : ids[Math.floor(rh() * ids.length)]
      const b = inThird[Math.floor(rh() * inThird.length)]
      if (a === b || dSet.has(key(a, b))) continue
      dSet.add(key(a, b))
      disease.push([a, b])
      added++
    }
    const netText = (pairs) =>
      ['Source\tTarget', ...pairs.map((p) => p.join('\t'))].join('\n') + '\n'
    const annText = demoToNormaTexts(base).annotation
    const dValues =
      [
        'Gene\tlog2FC\tpadj',
        ...ids.map((id) => {
          const boost = (moduleOf.get(id) || []).includes(third) ? 1.8 : 0
          const fc = Math.round((boost + gauss() * 0.7) * 1000) / 1000
          const p = Math.min(1, Math.pow(10, -Math.abs(fc) * (1 + rh() * 2)))
          return `${id}\t${fc}\t${p.toPrecision(3)}`
        }),
      ].join('\n') + '\n'
    NORMA_EXAMPLE_SETS['healthy-disease'] = {
      title: 'Healthy vs disease networks (simulated)',
      source: 'Simulated for NORMA 3.0',
      link: null,
      local: true,
      showAll: true,
      display: { edgeCurveStyle: 'bezier', legendShow: true, showGroupHulls: true },
      files: [
        {
          kind: 'network',
          name: 'Healthy (simulated)',
          fileName: 'healthy_network_simulated.txt',
          text: netText(healthy),
        },
        {
          kind: 'network',
          name: 'Disease (simulated)',
          fileName: 'disease_network_simulated.txt',
          text: netText(disease),
        },
        { kind: 'annotation', name: 'Modules', fileName: 'modules.txt', text: annText },
        {
          kind: 'colors',
          name: 'Disease vs healthy log2FC (simulated)',
          fileName: 'disease_vs_healthy_values_simulated.txt',
          text: dValues,
        },
      ],
    }
  })()

  // links between Help and About (and to the Welcome page) switch pages
  document.addEventListener('click', (e) => {
    const sample = e.target.closest('a[data-sample-link]')
    if (sample) {
      e.preventDefault()
      const q = new URLSearchParams(sample.getAttribute('href').replace(/^\?/, ''))
      const sel = document.getElementById('sampleSelect')
      sel.value = q.get('example')
      document.getElementById('btnSample').click()
      switchTab(q.get('tab') === '3d' ? 'network3d' : 'network')
      return
    }
    const a = e.target.closest(
      'a[href^="#help-"], a[href^="#about-"], a[href^="#api-"], a[href="#welcome"], [data-go-welcome]'
    )
    if (!a) return
    const href = a.getAttribute('href') || '#welcome'
    e.preventDefault()
    if (href === '#welcome') {
      switchTab('welcome')
      return
    }
    const id = href.slice(1)
    const page = id.startsWith('about-') ? 'about' : id.startsWith('api-') ? 'api' : 'help'
    if (currentTab !== page) switchTab(page, id)
    else {
      const t = document.getElementById(id)
      if (t) {
        t.scrollIntoView({ block: 'start' })
        t.classList.remove('flash')
        void t.offsetWidth
        t.classList.add('flash')
      }
    }
  })

  document.querySelectorAll('#tabs .tab').forEach((btn) => {
    btn.addEventListener('click', () => switchTab(btn.dataset.tab))
    btn.addEventListener('keydown', (ev) => {
      const tabs = [...document.querySelectorAll('#tabs .tab')]
      const i = tabs.indexOf(btn)
      let next = null
      if (ev.key === 'ArrowRight') next = tabs[(i + 1) % tabs.length]
      if (ev.key === 'ArrowLeft') next = tabs[(i - 1 + tabs.length) % tabs.length]
      if (next) {
        ev.preventDefault()
        next.focus()
        switchTab(next.dataset.tab)
      }
    })
  })

  document.querySelectorAll('.helplink').forEach((btn) => {
    btn.title = 'Open help for this section'
    btn.addEventListener('click', () => switchTab('help', btn.dataset.help))
  })

  document.querySelectorAll('.help-nav a').forEach((a) => {
    a.addEventListener('click', (ev) => {
      ev.preventDefault()
      const id = a.getAttribute('href').slice(1)
      switchTab('help', id)
    })
  })

  document.querySelectorAll('.help-body a[href^="#"]').forEach((a) => {
    a.addEventListener('click', (ev) => {
      ev.preventDefault()
      switchTab('help', a.getAttribute('href').slice(1))
    })
  })
}

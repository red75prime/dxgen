module annotations_DocumentTarget

open annotations

let documentTarget=[
  ("IPrintDocumentPackageStatusEventVtbl",IAManual, "IDispatchVtbl", [
    ("PackageStatusUpdated",[
      ("This",AThis);
      ("packageStatus",ANone);
    ],MANone);
  ]);
  ("IPrintDocumentPackageTargetFactoryVtbl",IAManual, "IUnknownVtbl", [
    ("CreateDocumentPackageTargetForPrintJob",[
      ("This",AThis);
      ("printerName",ANone);
      ("jobName",ANone);
      ("jobOutputStream",ANone);
      ("jobPrintTicketStream",ANone);
      ("docPackageTarget",ANone);
    ],MANone);
  ]);
  ("IPrintDocumentPackageTargetVtbl",IAManual, "IUnknownVtbl", [
    ("GetPackageTargetTypes",[
      ("This",AThis);
      ("targetCount",ANone);
      ("targetTypes",ANone);
    ],MANone);
    ("GetPackageTarget",[
      ("This",AThis);
      ("guidTargetType",ANone);
      ("riid",ANone);
      ("ppvTarget",ANone);
    ],MANone);
    ("Cancel",[
      ("This",AThis);
    ],MANone);
  ]);
  ]

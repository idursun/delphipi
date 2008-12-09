unit ProgressMonitor;

interface
uses PackageInfo;
type
  IProgressMonitor = interface
  ['{04616FA5-F839-4ADC-A4FF-2DDAAC02E597}']
    procedure Started;
    procedure Finished;
    procedure PackageProcessed(const packageInfo: TPackageInfo; status: TPackageStatus);
    procedure Log(const text: string);
    procedure CompilerOutput(const line: string);
  end;

implementation
end.

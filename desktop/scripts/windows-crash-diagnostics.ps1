param([switch]$Enable)

# Only used on disposable Windows CI runners with the bundled demo recording.
$folder = Join-Path $PWD 'test-results/native-crashes'
New-Item -ItemType Directory -Path $folder -Force | Out-Null
if ($Enable) {
  foreach ($exe in @('Rscript.exe', 'Rterm.exe')) {
    $key = "HKLM:\SOFTWARE\Microsoft\Windows\Windows Error Reporting\LocalDumps\$exe"
    New-Item -Path $key -Force | Out-Null
    New-ItemProperty -Path $key -Name DumpFolder -PropertyType ExpandString -Value $folder -Force | Out-Null
    New-ItemProperty -Path $key -Name DumpType -PropertyType DWord -Value 1 -Force | Out-Null
    New-ItemProperty -Path $key -Name DumpCount -PropertyType DWord -Value 2 -Force | Out-Null
  }
} else {
  $events = Get-WinEvent -FilterHashtable @{LogName='Application'; Id=1000,1001; StartTime=(Get-Date).AddHours(-1)} -ErrorAction SilentlyContinue |
    Where-Object Message -Match '(?i)Rscript\.exe|Rterm\.exe'
  $events | Format-List TimeCreated,Message | Out-File (Join-Path $folder 'windows-events.txt')
  if ($events) { Write-Output '::warning::Windows recorded a native R crash. See the native-crashes artifact diagnostics.' }
}

#  Project background
To ensure that my school work and dissertation were protected from hardware failures and enable version control, I developed a solution using a Python script. The script creates a compressed backup of my work and saves it onto a RAID drive, streamlining the backup process. To make the backup process automatic and consistent, I integrated the Python script into a batch file that can be scheduled to run on a weekly basis through Windows. As a result, I was able to rely on consistent and reliable backups of my work.

# Code

## Python script
```python
#! python3
# backupToZip.py - Copies an entire folder and its contents into
# a ZIP file whose filename increments.

import zipfile, os, datetime, shutil

# Backup the entire contents of "folder" into a ZIP file.

school_folder = 'C:\\Research'
raid1_folder = 'R:\\'
project_folder = 'C:\\Users\\[USER-NAME]\\[FOLDER-PATH]'
system_32 = 'C:\\Windows\\System32'

today = datetime.date.today()


def backupToZip(folder):
    # Backup the entire contents of "folder" into a ZIP file.

    folder = os.path.abspath(folder) # make sure folder is absolute

    # Figure out the filename this code should use based on
    # what files already exist.
    while True:
        zipFilename = os.path.basename(school_folder) + '_' + str(today) + '.zip'
        if not os.path.exists(zipFilename):
            break

    # Create the ZIP file.
    print('Creating %s...' % (zipFilename))
    backupZip = zipfile.ZipFile(zipFilename, 'w', allowZip64=True)

    # Walk the entire folder tree and compress the files in each folder.
    for foldername, subfolders, filenames in os.walk(folder):
        print('Adding files in %s...' % (foldername))
        # Add the current folder to the ZIP file.
        backupZip.write(foldername)
        # Add all the files in this folder to the ZIP file.
        for filename in filenames:
            newBase = os.path.basename(folder) + '_'
            if filename.startswith(newBase) and filename.endswith('.zip'):
                continue  # don't backup the backup ZIP files
            backupZip.write(os.path.join(foldername, filename))

    backupZip.close()

    print('Moving file to destination folder...')

    # Move file to raid1 storage
    shutil.move(system_32 + '\\' + zipFilename, raid1_folder)

    print('Done.')

backupToZip(school_folder)
```

## Batch file
```batch
@echo off
python C:\Users\[USER-NAME]\[FOLDER-PATH]\School_Backup.py %*
pause
```
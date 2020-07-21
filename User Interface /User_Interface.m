%% Userinterface script for creating mean activation tables (mean_activation.m), asymmetry tables based on mean activation (AI.m), and FWHM calculations (ImgAI3.m)
%Run with functions: activ_table5.m, AI.m, ImgAI3.m
%Specifics for our project: (enter in commmand window before running script)
%X = '/Volumes/cerebro/Studies/KLU_APC2/Public/Analysis/data/';
%XX = '/step03_FaceNames/FirstLevel/con_0003.nii';
%Xroi = dir('/Volumes/cerebro/Studies/KLU_APC2/Public/Analysis/misc/ROIs');
%ROI = {'rHippocampus_L_37.nii','rHippocampus_R_38.nii','LeftBA46.img','RightBA46.img'};

%Elizabeth Mountz - ejm122@pitt.edu

%% Gather Directory Input:
X = input('Please enter the directory path preceding the subject id number, starting and ending with the file separation character');
XX = input('Please enter the directory path following the subject id and scan number startitng with the file separation character');
Xroi = input('Please enter the directory for the ROI folder');
ROI = input('Please enter the file names for all relevant ROIs');

%% Make subject text file
answer = input('Would you like to find the mean activation for all subjects at the given directory? Enter Y or N','s');
if answer == 'N'
    subject_txt = input('Please enter the file name with the list of desired subject numbers/scan numbers. Include the .txt extension.','s');
elseif answer =='Y'
        X_dir = dir([X,'9*/8*',XX]);
  
    for n = 1:length(X_dir)
        str = X_dir(n).folder; %header's string
        out = regexp(str,filesep,'split'); %replaced '/' with filesep
        m(n,:) = out(:);
    end
  
    [rows,col] = size(m);
    subject_scan = zeros(rows, 2);
    subj_id_index = find(startsWith(m(1,:), '9'));
    scan_id_index = find(startsWith(m(1,:), '8'));
    for i = 1:rows
      subj_id = m(i,subj_id_index);
      subject_scan(i,1) = str2num(char(subj_id));
      scan_id = m(i,scan_id_index);
      subject_scan(i,2) = str2num(char(scan_id));
    end
    subject_txt = fopen('subject_txt.txt','wt');
    for i = 1:rows
    fprintf(subject_txt,'%d%c%d\n',subject_scan(i,1), filesep, subject_scan(i,2));
    end
    subject_txt = 'subject_txt.txt';
else
    disp('Error - invalid input')
    return
end
 
 subject = importdata(subject_txt,filesep);
 %% Make table of mean activation for each subject and ROI
 mean_activ = mean_activation(X,XX,Xroi,ROI,subject);
%% Save mean activation table:
%Answers = {'L_Hippo_Activ', 'R_Hippo_Activ','L_DLPFC_Activ', 'R_DLPFC_Activ'};
fprintf("\n When prompted, list the variable names for the ROI activation vectors one at a time, in the same order as provided in the ROI directory input \n")

%Input Column header names
Answers_Activ = {length(ROI)};
for K = 1 : length(ROI)
  Answers_Activ{K} = input('List a variable name for the ROI activation vector', 's');
end

firstColumns = {'Subject_ID','Scan_ID'}; %Constant first 2 columns
ColumnNames = [firstColumns, Answers_Activ]; %Concatenate first 2 columns and user ROI column names
mean_activ_table = array2table(mean_activ, 'VariableNames', ColumnNames); %Make table
disp(mean_activ_table)
writetable(mean_activ_table,'activ_values.txt','Delimiter',',','WriteVariableNames', true); %Save Table 

%Mean activation values now saved in table 'activ_values.txt'
%% Asymmetry Index Calculation Function
Asy = [subject(:,1), subject(:,2)];
count = 0;
Answer = input('Would you like to calculate the Asymmetry Index for an ROI? Enter Y or N','s');
if Answer == 'Y'
    fprintf('\n When prompted, Enter activation vectors based on the array mean_activ, displayed as a table above. \n')
    while Answer == 'Y'
        count = count + 1;
        L= input('Enter the vector for left mean activation for desired ROI');
        R = input('Enter the vector for right mean activation for desired ROI');
        Asy= [Asy,AI(L,R)];
        Answer = input('Would you like to calculate another Asymmetry Index for an ROI? Enter Y or N','s');
    end
%% Save Asymmetry Table
    %Answers = {'Hippocampus_AI', 'DLPFC_AI'};
    fprintf("\n When prompted, list the variable name for the ROI asymmetry vectors one at a time, in the same order as provided in the creation of the AI table \n")

    %Input Column header names
    Answers_Asy = {length(ROI)};
    for K = 1 : count
      Answers_Asy{K} = input('List the variable name for the ROI asymmetry vector', 's');
    end

    firstColumns = {'Subject_ID','Scan_ID'}; %Constant first 2 columns
    ColumnNames = [firstColumns, Answers_Asy]; %Concatenate first 2 columns and user ROI column names
    AI_table = array2table(Asy, 'VariableNames', ColumnNames); %Make table
    disp(AI_table)
    writetable(AI_table,'AI.txt','Delimiter',',','WriteVariableNames', true); %Save Table 
end

%% FWHM Calculation Function
answer = input('Would you like to calculate the FWHM for all ROI? Enter Y or N','s');
if answer == 'Y'
    activ_deactiv_radius = ImgAI3_User_Interface(X,XX,Xroi,ROI,subject,true);
    %%Save FWHM Table

    %Answers = {'Left_Hippocampus_FWHM', 'Right_Hippocampus_FWHM', 'Left_DLPFC_FWHM','Right_DLPFC_FWHM'};
        fprintf("\n When prompted, list the variable name for the ROI FWHM one at a time, in the same order as provided in the ROI directory. \n")

        %Input Column header names
        Answers_FWHM = {length(ROI)};
        for K = 1 : length(ROI)
          Answers_FWHM{K} = input('List the variable name for an ROI FWHM', 's');
        end

        firstColumns = {'Subject_ID','Scan_ID'}; %Constant frst 2 columns
        ColumnNames = [firstColumns, Answers_FWHM]; %Concatenate first 2 columns and user ROI column names
        FWHM_Table = array2table(activ_deactiv_radius, 'VariableNames', ColumnNames); %Make table
        disp(FWHM_Table)
        writetable(FWHM_Table,'FWHM.txt','Delimiter',',','WriteVariableNames', true); %Save Table 
end
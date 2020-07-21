%%This function takes in study's fMRI directory, ROI directory, ROI name
%and subject file. Then the function outputs mean activation for the
%subejcts at ROIs as well as the asymmetry index values. Make sure you
%have AI.m and subject csv file in your current directory before running.
%Additionally, the code will check whether the fRMI file exists in
%the server.
% - Jinghang Li (jil202@pitt.edu) June 25th, 2020

function [mean_activ] = mean_activation(X,XX,Xroi,ROI,subject)

X = repmat(X,length(subject),1); %making sure the dimension matches
XX = repmat(XX,length(subject),1); %making sure the dimension matches
mri_dir = [X,num2str(subject(:,1)),repmat('/',length(subject),1),num2str(subject(:,2)),XX]; %mri_dir is all the directories for specified mri scans
for i =1:length(mri_dir(:,1))
    if ~exist(mri_dir(i,:),'file') == 1 %if there is no fMRI scan then skip the folder
        continue
    end
mri(i) = dir(mri_dir(i,:));
end
mri(all(cell2mat(arrayfun(@(x) structfun(@isempty, x), mri, 'UniformOutput', false)),1)) = []; % remove the empty rows in the struct

path =[filesep 'Volumes',filesep,'tubby',filesep,'Processing',filesep,'Scripts',filesep,'GPN_Toolbox'];
cd (path)
addpath(genpath(['Volumes',filesep,'tubby',filesep,'Processing',filesep,'Scripts',filesep,'GPN_Toolbox']))
mean_activ = zeros(length(mri), length(ROI)+2); %900#; 800#; MeanActivation_L_hippo; MeanActivation_R_hippo; L_dLPFC; R_dLPFC
%% Save all images
for i = 1:length(mri)
    hdr(i) = load_untouch_nii([mri(i).folder  filesep mri(i).name]);
end
%% Specified subjects
fprintf('Analysis starts here:....\n')
fprintf('Reading fMRI scans for specified patients........\n')

mean_activ(:,1) = subject(:,1);
mean_activ(:,2) = subject(:,2);

subj_num = length(unique(mean_activ(:,1))); %getting the number of subjects
scan_num = length(unique(mean_activ(:,2))); %getting the number of total scans
fprintf('The table has %d different patients. The total number of fMRI scan is %d \n', subj_num, scan_num);

%% ROI
%ROI = {'rHippocampus_L_37.nii','rHippocampus_R_38.nii','LeftBA46.img','RightBA46.img'};
fprintf('Calculating mean activation in the ROI regions for all patients.......\n')
tic
for i =1:length(ROI) %getting ROI masks
    mask = load_untouch_nii([Xroi(1).folder,filesep,ROI{i}]);
    ROI_mask{i} = mask.img;
    for j = 1:length(hdr) %using the ROI masks getting the wanted mean activation
        mean = nanmean(hdr(j).img((abs(ROI_mask{i}) > 0)));
        mean_activ(j,i+2) = mean;
    end
end
toc  

end

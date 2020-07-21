%%ImgAI3 fMRI Image based spatial activation asymmetry analysis
%%This function takes in study's fMRI directory, ROI directory, ROI names,
%subject file, as well as the option viewing the curv fitting plots. 
%Then the function outputs Full Width @ Half Maximum (FWHM) of the 
%activated & deactivated region for the subejcts at ROIs as well as the 
%asymmetry index values. 
%Make sure you have AI.m and subject csv file in your current directory before running.
%Additionally, the code will check whether the fRMI file exists in
%the server.
%
%
%%Example
%Type the following command in the MATLAB window
% X = '/Volumes/cerebro/Studies/KLU_APC2/Public/Analysis/data/';
% XX = '/step03_FaceNames/FirstLevel/con_0003.nii';
% Xroi = dir('/Volumes/cerebro/Studies/KLU_APC2/Public/Analysis/misc/ROIs');
% ROI = {'rHippocampus_L_37.nii','rHippocampus_R_38.nii','LeftBA46.img','RightBA46.img'};
% subject = csvread('list.txt');
% [activ_deactiv_radius] = ImgAI3(X,XX,Xroi,ROI,subject,true);
%
% - Jinghang Li (jil202@pitt.edu) July 3rd, 2020
function [activ_deactiv_radius] = ImgAI3(X,XX,Xroi,ROI,subject,option)
% X = '/Volumes/cerebro/Studies/KLU_APC2/Public/Analysis/data/';
% XX = '/step03_FaceNames/FirstLevel/con_0003.nii';
% Xroi = dir('/Volumes/cerebro/Studies/KLU_APC2/Public/Analysis/misc/ROIs'); %ROI directory
% ROI = {'rHippocampus_L_37.nii','rHippocampus_R_38.nii','LeftBA46.img','RightBA46.img'}; %ROI names
% subject = csvread("list.txt");

%make sure the file directories dimensions are correct for dir
X = repmat(X,length(subject),1);
XX = repmat(XX,length(subject),1);
mri_dir = [X,num2str(subject(:,1)),repmat('/',length(subject),1),num2str(subject(:,2)),XX];
%check if the file exists in the server
for i =1:length(mri_dir(:,1))
    if ~exist(mri_dir(i,:),'file') == 1 %if there is no fMRI scan then skip the folder
        continue
    end
mri(i) = dir(mri_dir(i,:)); %getting dir for the fMRI files
end
mri(all(cell2mat(arrayfun(@(x) structfun(@isempty, x), mri, 'UniformOutput', false)),1)) = []; % remove the empty rows in the struct
path =[filesep 'Volumes',filesep,'tubby',filesep,'Processing',filesep,'Scripts',filesep,'GPN_Toolbox']; %specifying path for matlab toolbox
cd (path)
addpath(genpath(['Volumes',filesep,'tubby',filesep,'Processing',filesep,'Scripts',filesep,'GPN_Toolbox']))
%% Reading fMRI images
fprintf('Reading all functional MRI iamges.....\n')
tic
for i = 1:length(mri) %for loop loading the specified fMRI images
    hdr(i) = load_untouch_nii([mri(i).folder filesep mri(i).name]); %hdr struct contains all the fMRI information
end
toc
fprintf('fMRI images reading Done.....\n')
%% Reading ROIs
fprintf('Reading ROI masks.......\n')
for i = 1:length(ROI)
    mask = load_untouch_nii([Xroi(1).folder,filesep,ROI{i}]); %mask struct contains all the ROI information
    ROI_mask{i} = mask.img; %ROI 3D matrix
end
fprintf('ROI masks reading done!.........\n')
%% Anchoring at the polar activation values (Positive & Negative) & marking the deactivated vs activated 
fprintf('Putting down anchor matrix for all subjects at all ROIs......\n')
tic
region = {}; %region is the activation of subjects fMRI at ROIs
anchor = {}; %loacation cell of the polar value
for i = 1:length(mri) %looping through all the subject's existed fMRI files
    for j = 1:length(ROI_mask) %looping through specified region of interest
%setting anchor down at the extream activation position (positive and negative)        
region{i,j} = abs(hdr(i).img .* (ROI_mask{j} > 0)); %region is the activation of subjects fMRI at ROIs
[x,y,z] = ind2sub(size(region{i,j}), find (region{i,j} == max(region{i,j}(:)))); %finding the spatial index of the polar activation value
loc{i,j} = [x,y,z]; %store the location for later usage in estimating FWHM (ceiling effect)
if hdr(i).img(x,y,z) > 0 %marking down the deactivated and activated
polar_index(i,j) = 1; %if the region is activated during task, the marker is 1
elseif hdr(i).img(x,y,z) < 0
polar_index(i,j) = -1; %if the region is deactivated during task, the marker is -1
end
%creating anchor matrix
[a,b,c] = size(region{i,j}); % creating an empty array the same size as the fMRI image
anchor{i,j} = zeros(a,b,c);  %anchoring the max activation at the ROI
anchor{i,j}(x,y,z) = 1; %only the voxel that has the most negative or the most positve value is marked as 1
    end
end
toc
fprintf('Anchoring DONE!......\n')
%% Dilating anchor matrix & computing mean activation @ the dilated spheres
R = 25; % setting dilating radius for imdilate
fprintf('Dilating anchor matrix for all subjects at all ROIs......\n')
fprintf('Computing activation mean within the dilated spheres at all ROIs......\n')
fprintf('This may take a couple of minutes. Take a break and come back later.........\n')
for i = 1:length(mri) %looping through all the subject's existed fMRI files
    i %counter to see the imdilating progress
    for j = 1:length(ROI_mask) %looping through specified region of interest
dilated = imdilate(anchor{i,j},strel('sphere',1)); %dilate all the anchors first
for r = 1:R %additional dilating each anchor (outer ring dilation to save computational resources)
dilated = imdilate(dilated,strel('sphere',1)); 
dilated_store{i,j,r} = dilated;
sphere_activ{i,j,r} = dilated_store{i,j,r} .* (hdr(i).img .* (ROI_mask{j} > 0)); %ready for mean activation calculation
mean_r{i,j,r} = nanmean(sphere_activ{i,j,r}(abs(sphere_activ{i,j,r})>0)); %compute the nanmean for each of the dilated spheres
end
    end
end
fprintf('Dilated sphere activation mean computation Done!......\n')
%% Sending SMS messages when the computation is done!
%=====================================================================
% cd /Users/gpnuser/Downloads/Matlab_Functions/ImgAI
% send_text_message('412-295-9503','Sprint','Mean activation at dilated spheres computation is done!')
%=====================================================================
%% Solving FWHM & curv fitting visulization (optional)
fprintf('Estimating FWHM for subjects at ROIs..............\n')
count =1; %frame counter for writing video file
%data visulization
for i = 1:length(mri) %looping through all the subject's existed fMRI files
    for j = 1:length(ROI_mask) %looping through specified region of interest
        r = (1:1:R); %radius will be the horizontal axis for plotting
        mag = reshape(mean_r(i,j,:),1,R); %the magnitude of the mean activation for all dilated spheres
        mag = double(cell2mat(mag)); %convert to variable type double
        mag2 = repmat(mag(1)/2,1,R); %identify half max value
        f1=fit(r',mag','linearinterp'); %using linearinterp model to fit all calculated mean activation magnitude
        f2=fit(r',mag2','linearinterp'); %f2 is the function saying y = half max. it is fitted using linearinterp because fzero solver needs it to be a function 
%solving the FWHM
Radius(i,j) = fzero(@(r) f1(r) - f2(r) ,2); 
if Radius(i,j) > R || isnan(Radius(i,j)) == 1 %if the subject's mean activation magnitudes are all greater than the half max value or fzero doesn't return a solution then the codes below are approximating the maximum distance between the anchor and the ROI edges
position = loc{i,j}; %variable position is 3D matrix location where the subject's most positive or most negative voxel activation value is at. 
[roix,roiy,roiz] = ind2sub(size(ROI_mask{j}), find(ROI_mask{j} > 0));% 3d matrix index where ROI is 1.
coor = [roix,roiy,roiz]; %3d matrix index where ROI is located 
Radius(i,j) = max(sqrt(sum((coor - position).^2,2))); %finding the maximum distance between the anchor and the ROI 
fprintf('The fzero solver is skipped for the subject. The max distance from the anchor to the ROI edge is calculated instead \n'); %printing out in the command window, indicating there is a ceiling effect
end        
%if the plot option is toggled on then plotting all the curve fitting  
if option == true
    if i == 1 && j ==1
    fprintf('Press any key to see the curve fitting plots. \n')
    pause()
    end
plot(r,mag,'ro') %calculated mean activation magnitude of all the dilated spheres
hold on 
plot(r,f1(r)) %plotting the fitted curve
hold on 
plot(Radius(i,j),mag(1)/2,'kx','MarkerSize',10) %plotting the solution of where the estimated FWHM is at
hold on
plot(r,repmat(mag(1)/2,1,R),'b--') %dashed line of the half max value
hold off
txt = ['Subject: ',num2str(i),' ROI: ', num2str(j), 'FWHM is: ', num2str(Radius(i,j))]; %title the plot
title(txt)
F(count) = getframe(gcf);
count = count + 1;
elseif option == false %if the option is false then no curve fitting plots will be shown
    continue
end
    end
end
fprintf('FWHM estimating done!..............\n')
%% Writing a video file for the fitted curve plots
% v = VideoWriter('FittedCurve.avi');
% v.FrameRate = 2;
% open(v)
% for k = 1:length(F) 
%    frame = F(k).cdata;
%    writeVideo(v,frame);
% end
% close(v)
%% Asymmetry index calcualtion based on half max (consider convert the distance at negative activation to be negative?)
fprintf('Appending negative sign to the deactivated FWHM.........\n')
activ_deactiv_radius = zeros(length(mri),length(ROI_mask)+2);
activ_deactiv_radius(:,3:end) = Radius .* polar_index; %appending the negative sign to the estimated FWHM indicating the deactivated region's radius

%the for loop below appends 900 and 800 ID to the FWHM radius matrix
for i = 1:length(mri)
    str = hdr(i).fileprefix; %header's string
    out = regexp(str, filesep ,'split');
    activ_deactiv_radius(i,1) = str2num(cell2mat(out(1,9)));
    activ_deactiv_radius(i,2) = str2num(cell2mat(out(1,10)));
end
fprintf('DONE!..............\n')
end
%% Questions

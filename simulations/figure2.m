clear all;
close all;
clc;
global Save GraphDir;
addpath('~/export_fig_dir');
root=(pwd);
InputDirRaw = '~/Documents/GitHub/CGMV_prestamos/stata/output';
OutputDir   = '~/Documents/GitHub/CGMV_prestamos/paper_figures';
ModelDir    = '~/Documents/GitHub/CGMV_prestamos/simulations';

% Options for this run;

Save = 0;% Save graphs;

mygray=[.7 .7 .7];
myblue=[51/255 102/255 153/255];
lightblue=[102/255 102/255 1];
lightred=[249/255 80/255 80/255];
%% RUN MODELS
agevec=[19 19 19 22];
agemax = 60;

for i_s=1:1,
    for i_e=4:4,
        close all;
        
        % CHOOSE MODEL;
        sex = i_s;
        ed = i_e;
        
        agemin = agevec(ed);
        ModelDir = [ModelDir '/sex_' int2str(sex) '/edgroup_' int2str(ed)];
        cd(OutputDir);
        
        %% LOAD RAW DATA
            
            % raw age profiles (data) -- from EmpStatus.do
            for e = 4:4
                for s = 1:1
                    ageprofiles{e,s} = importdata([InputDirRaw '/ageprofiles_s' int2str(s) '_e' int2str(e) '.txt']);
                    for i = 1:size(ageprofiles{e,s}.data,2)
                        ageprofiles{e,s}.(ageprofiles{e,s}.textdata{i}) = ageprofiles{e,s}.data(:,i);
                    end
                end
            end
            
            age = ageprofiles{ed,sex}.age;
            nage = length(age);
            
            % Distribution of observables in first stage -- from FirstStage.do
            for e = 4:4
                for s = 1:1
                    constantdistn{e,s} = importdata([InputDirRaw '/constantdistn_s' int2str(s) '_e' int2str(e) '.txt']);
                    constantdistn{e,s}(:,2) = constantdistn{e,s}(:,2)./sum(constantdistn{e,s}(:,2));
                end
            end
        
        % LOAD QUANTILES
            
            for e = 4:4
                for s = 1:1
                    quantileslog{e,s} = importdata([InputDirRaw '/quantileslog_s' int2str(s) '_e' int2str(e) '.txt']);
                    for i = 1:size(quantileslog{e,s}.data,2)
                        quantileslog{e,s}.(quantileslog{e,s}.textdata{i}) = quantileslog{e,s}.data(:,i);
                    end
                end
            end
            
            PClrearns = [quantileslog{ed,sex}.lp5 quantileslog{ed,sex}.lp10 quantileslog{ed,sex}.lp25 quantileslog{ed,sex}.lp50 quantileslog{ed,sex}.lp75 quantileslog{ed,sex}.lp90 quantileslog{ed,sex}.lp95 ];
            
            ysim = load([ModelDir '/ysim.txt']);
            expysim = load([ModelDir '/expysim.txt']);
            uesim = load([ModelDir '/uesim.txt']);
            statusM=uesim;
            nsim = size(ysim,1);
            groupsim = randp(constantdistn{ed,sex}(:,2),nsim,1);
            constantsim = constantdistn{ed,sex}(groupsim,1) ;
            lrearnsM = ysim + constantsim*ones(1,nage);
            lrearnsM(statusM==1) = NaN;
        
    end
    
    
    %% QUANTILES
    screens=get(0,'monitorpositions');
    nomonitors = size(screens,1);
    posgraphs = screens(end,: );
        PClrearnsM = prctile(lrearnsM,[5 10 25 50 75 90 95])';
        
        figure('color','white','position',posgraphs); hold on;
        plot(age,PClrearns,'LineWidth',2.5);
        plot(age,PClrearnsM,'--','LineWidth',1.5);
        grid;
        axis([ageprofiles{ed,sex}.age(1)-1 ageprofiles{ed,sex}.age(end)+1 ...
            min(PClrearnsM(:,1))-.5 max(PClrearnsM(:,7))+.5]);
        set(gca,'ticklabelinterpreter','latex','fontsize',20);
        lg=legend('5','10','25','50','75','90','95','Location','SouthEast');
        set(lg,'interpreter','latex');
        ylabel('Log Earnings','interpreter','latex','Fontsize',20);
        xlabel('Age','interpreter','latex','Fontsize',20);
        if Save==1
            export_fig([GraphDir '/lquantiles_s' int2str(sex) '_e' int2str(ed)],'-eps');
        end

        
    
end

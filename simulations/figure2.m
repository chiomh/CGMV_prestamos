clear all;
close all;
clc;
global Save GraphDir;
addpath('~/export_fig_dir');
root=(pwd);
InputDirRaw = '~/Dropbox/research/cgm_dynamics/CGM_local/stata/output';
OutputDir   = '~/Dropbox/research/cgm_dynamics/CGM_local/matlab';
GraphDir    = '~/Dropbox/research/cgm_dynamics/CGM_local/matlab/graphs';

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
        ModelDir    = '~/Dropbox/research/cgm_dynamics/CGM_local/fortran/simulations/Dual';
        % CHOOSE MODEL;
        sex = i_s;
        ed = i_e;
        
        agemin = agevec(ed);
        ModelDir = [ModelDir '/sex_' int2str(sex) '/edgroup_' int2str(ed)];
        cd(OutputDir);
        
        %% LOAD RAW DATA
        if DoRawData==1
            
            % raw age profiles (data) -- from EmpStatus.do
            for e = 1:4
                for s = 1:2
                    ageprofiles{e,s} = importdata([InputDirRaw '/ageprofiles_s' int2str(s) '_e' int2str(e) '.txt']);
                    for i = 1:size(ageprofiles{e,s}.data,2)
                        ageprofiles{e,s}.(ageprofiles{e,s}.textdata{i}) = ageprofiles{e,s}.data(:,i);
                    end
                end
            end
            
            age = ageprofiles{ed,sex}.age;
            nage = length(age);
            
            % Distribution of observables in first stage -- from FirstStage.do
            for e = 1:4
                for s = 1:2
                    constantdistn{e,s} = importdata([InputDirRaw '/constantdistn_s' int2str(s) '_e' int2str(e) '.txt']);
                    constantdistn{e,s}(:,2) = constantdistn{e,s}(:,2)./sum(constantdistn{e,s}(:,2));
                end
            end
        end
        
        % LOAD QUANTILES
        if DoQuantiles==1
            %NB: these are nonparametric but do not control for first stage
            for e = 1:4
                for s = 1:2
                    quantilesraw{e,s} = importdata([InputDirRaw '/quantilesraw_s' int2str(s) '_e' int2str(e) '.txt']);
                    for i = 1:size(quantilesraw{e,s}.data,2)
                        quantilesraw{e,s}.(quantilesraw{e,s}.textdata{i}) = quantilesraw{e,s}.data(:,i);
                    end
                end
            end
            
            for e = 1:4
                for s = 1:2
                    quantileslog{e,s} = importdata([InputDirRaw '/quantileslog_s' int2str(s) '_e' int2str(e) '.txt']);
                    for i = 1:size(quantileslog{e,s}.data,2)
                        quantileslog{e,s}.(quantileslog{e,s}.textdata{i}) = quantileslog{e,s}.data(:,i);
                    end
                end
            end
            
            PCrearns = [quantilesraw{ed,sex}.p5 quantilesraw{ed,sex}.p10 quantilesraw{ed,sex}.p25 quantilesraw{ed,sex}.p50 quantilesraw{ed,sex}.p75 quantilesraw{ed,sex}.p90 quantilesraw{ed,sex}.p95 ];
            PClrearns = [quantileslog{ed,sex}.lp5 quantileslog{ed,sex}.lp10 quantileslog{ed,sex}.lp25 quantileslog{ed,sex}.lp50 quantileslog{ed,sex}.lp75 quantileslog{ed,sex}.lp90 quantileslog{ed,sex}.lp95 ];
        end
        
    end
    %% QUANTILES
    screens=get(0,'monitorpositions');
    nomonitors = size(screens,1);
    posgraphs = screens(end,: );
    if DoQuantiles==1
        PClrearnsM = prctile(lrearnsM,[5 10 25 50 75 90 95])';
        PCrearnsM = prctile(rearnsM,[5 10 25 50 75 90 95])';
        
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
        
        figure('color','white','position',posgraphs); hold on;
        plot(age,PCrearns,'LineWidth',2);
        plot(age,PCrearnsM,'--','LineWidth',1);
        grid;
        axis([ageprofiles{ed,sex}.age(1)-1 ageprofiles{ed,sex}.age(end)+1 ...
            min(PCrearnsM(:,1)) max(PCrearnsM(:,7))]);
        set(gca,'ticklabelinterpreter','latex','fontsize',20);
        lg=legend('5','10','25','50','75','90','95','Location','Northwest');
        set(lg,'interpreter','latex');
        ylabel('Real Earnings','interpreter','latex','Fontsize',20);
        xlabel('Age','interpreter','latex','Fontsize',20);
        if Save==1
            export_fig([GraphDir '/quantiles_s' int2str(sex) '_e' int2str(ed)],'-eps');
        end
        
    end
    
end

clear; close all;clc; 


tab1 = readtable("test3.csv");


%Set labels, legends, and numbers to latex
set(0,'defaulttextinterpreter','latex');
set(0,'defaultAxesTickLabelInterpreter','latex');



% Create figure
fig1 = figure('color','white');
plot(tab1.x, tab1.y, 'r','linewidth', 1.5);
xlabel('$\eta$')
ylabel('$\theta$')
grid on

exportgraphics(fig1, 'test3.png', 'Resolution', 800')
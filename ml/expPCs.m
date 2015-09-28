% exploring variations in principal components across populations with
% different parameters and sizes


points = -1:.01:1;

%%
rng(1)
figure 

n = [50 1000];
reps = 10;

repPCs = zeros(length(points), reps);
for i = 1:length(n)
    subplot(1,length(n), i)
    for j = 1:reps
        sg = getPMSpikeGenerators(n(i));

        pop = CosinePopulation(1, sg{6}, 'test');
        rates = getRates(pop, points, 0, 0);

        [U, S, V] = svd(rates);

        ind = 3;
        if V(1,ind) > V(100,ind)
            repPCs(:,j) = V(:,ind);
        else 
            repPCs(:,j) = -V(:,ind);
        end
    end
    plot(points, repPCs, 'k')
    set(gca, 'FontSize', 18)
    xlabel('$x$', 'FontSize', 24, 'Interpreter', 'LaTex')
    set(gca, 'YLim', [-.12 .17])
end
set(gcf, 'Position', [440 479 690 319])

%%
rng(1)
sg = getPMSpikeGenerators(1000);
sgInd = [1 6 7 10];
npc = 6;
figure
for i = 1:length(sgInd)
    pop = CosinePopulation(1, sg{sgInd(i)}, 'test');
    rates = getRates(pop, points, 0, 0);
    [U, S, V] = svd(rates);
    
    PCs = V(:,1:npc);
    if i == 1 
        firstPCs = PCs;
    else % change signs of PCs as needed to match first population
        prod = PCs' * firstPCs;
        for j = 1:npc
            if prod(j,j) < 0
                PCs(:,j) = -PCs(:,j);
            end
        end
    end
    
    subplot(2, length(sgInd), i)
    plot(points, rates(1:50,:), 'k')
    set(gca, 'XTick', [])   
    if i == 1, ylabel('Spike Rate (spikes/s)', 'FontSize', 18), end
    set(gca, 'FontSize', 18)
    
    subplot(2, length(sgInd), length(sgInd)+i)
    plot(points, PCs, 'k')
    set(gca, 'YLim', [-.18 .18])
    if i == 1, ylabel('PCs', 'FontSize', 18), end
    set(gca, 'FontSize', 18)
    xlabel('$x$', 'FontSize', 24, 'Interpreter', 'LaTex')
end
set(gcf, 'Position', [440 414 792 384])

%%
rng(1)
target = sin(3*pi*points);
npc = 15;

sg = getPMSpikeGenerators(100);
pop = CosinePopulation(1, sg{6}, 'test');
rates = getRates(pop, points, 0, 0);
[U, S, V] = svd(rates);

smallRelNoise = .001; % small for decoding PCs
relNoise = .05;
gamma = rates * rates';
gamma = gamma + (smallRelNoise*max(rates(:)))^2*size(rates,2) * eye(size(gamma,1));
noiseVar = zeros(1,npc); %expected per PC
for i = 1:npc
    PC = V(:,i);
    decoders = gamma \ (rates * PC);  
%     plot(points, PC), hold on, plot(points, rates'*decoders, 'r--'), hold off
%     norm(decoders)
    noiseVar(i) = var((relNoise*max(rates(:))*decoders).^2);
end

% decode target from neurons 
gamma = rates * rates';
gamma = gamma + (relNoise*max(rates(:)))^2*size(rates,2) * eye(size(gamma,1));
decoders = gamma \ (rates * target'); 
neuronApprox = rates' * decoders;

% decode target from noisy PCs
PCs = V(:,1:npc)';
gamma = PCs * PCs';
gamma = gamma + diag(noiseVar)*length(points);
decoders = gamma \ (PCs * target'); 
pcApprox = PCs' * decoders;

% decode target from noisy bigger-but-similar population PCs
sg = getPMSpikeGenerators(500);
pop = CosinePopulation(1, sg{6}, 'test');
rates = getRates(pop, points, 0, 0);
[U, S, V] = svd(rates);
PCs = V(:,1:npc)';
gamma = PCs * PCs';
gamma = gamma + diag(noiseVar)*length(points);
decoders = gamma \ (PCs * target'); 
similarApprox = PCs' * decoders;

% decode target from noisy bigger-but-different population PCs
sg = getPMSpikeGenerators(100);
pop = CosinePopulation(1, sg{1}, 'test'); % note different params 
rates = getRates(pop, points, 0, 0);
[U, S, V] = svd(rates);
PCs = V(:,1:npc)';
gamma = PCs * PCs';
gamma = gamma + diag(noiseVar)*length(points);
decoders = gamma \ (PCs * target'); 
differentApprox = PCs' * decoders;

subplot(1,2,2)
hold on
plot(points, neuronApprox-target'+.15, 'k', 'LineWidth', 2)
plot(points, pcApprox-target'+.05, 'Color', [.5 .5 .5], 'LineWidth', 2)
plot(points, similarApprox-target'-.05, 'k--', 'LineWidth', 2)
plot(points, differentApprox-target'-.15, 'Color', [.5 .5 .5], 'LineStyle', '--', 'LineWidth', 2)
% legend('Neurons', 'PCs', 'PCs (more neurons)', 'PCs (different parameters)')
% plot([.75 .75], [-.35 -.25], 'k', 'LineWidth', 3)
set(gca, 'FontSize', 18)
set(gca, 'YTick', [])
xlabel('$x$', 'FontSize', 24, 'Interpreter', 'LaTex')

set(gcf, 'Position', [440 414 792 384])
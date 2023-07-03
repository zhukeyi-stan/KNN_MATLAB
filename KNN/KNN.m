function [ids,dists]=KNN(newPnt,pnts,num)
    dim=0;
    kdtree{1}=pnts;
    currentNode=1;
    ids=[];
    dists=[];
    visitedList=[];
    while(true)
        dim=mod(floor(log(currentNode)/log(2)),size(pnts,2))+1;
        if(size(kdtree{currentNode},1)>1)
            med=median(kdtree{currentNode}(:,dim));
            [~,I]=sort(kdtree{currentNode}(:,dim));
            pickedPntID=I(ceil(max(size(I))/2));
            pickedPnt=kdtree{currentNode}(pickedPntID,:);
            kdtree{currentNode}(pickedPntID,:)=[];
            kdtree{currentNode*2}=kdtree{currentNode}(kdtree{currentNode}(:,dim)<=med,:);
            kdtree{currentNode*2+1}=kdtree{currentNode}(kdtree{currentNode}(:,dim)>med,:);
            kdtree{currentNode}=pickedPnt;
            if(newPnt(dim)<med && ~isempty(kdtree{currentNode*2}))
                currentNode=currentNode*2;
            else
                currentNode=currentNode*2+1;
            end
        else
            if(~ismember(currentNode,visitedList))
                visitedList=union(visitedList,currentNode);
                if(size(ids,2)<num)
                    ids=[ids currentNode];
                    dists=[dists norm(newPnt-kdtree{currentNode})];
                else
                    tempDist=norm(newPnt-kdtree{currentNode});
                    if tempDist<dists(end)
                        dists(end)=tempDist;
                        ids(end)=currentNode;
                    end
                end
                [dists,inds]=sort(dists,'ascend');
                ids=ids(inds);
                %%%Decide next node
                if(abs(newPnt(dim)-kdtree{currentNode}(dim))<dists(end))
                    if(max(size(kdtree))>2*currentNode+1 && ~isempty(kdtree{2*currentNode+1}) && ~ismember(2*currentNode+1,visitedList))
                        currentNode=2*currentNode+1;
                    elseif(max(size(kdtree))>2*currentNode && ~isempty(kdtree{2*currentNode}) && ~ismember(2*currentNode,visitedList))
                        currentNode=2*currentNode;
                    % else
                        
                    end
                else
                    if(currentNode==1)
                        break
                    end
                    currentNode=floor(currentNode/2);
                end    
            elseif(currentNode~=1)
                currentNode=floor(currentNode/2);
            else
                break
            end
        end
    end
    for i=1:size(ids,2)
        ids(i)=find(ismember(pnts,kdtree{ids(i)},"rows"));
    end
end
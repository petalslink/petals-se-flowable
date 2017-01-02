/**
 * Copyright (c) 2015-2016 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.activitibpmn.incoming.integration;

import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_OP_GETTASKS;

import java.net.URI;
import java.util.List;
import java.util.logging.Logger;

import org.activiti.engine.RepositoryService;
import org.activiti.engine.TaskService;
import org.activiti.engine.repository.ProcessDefinition;
import org.activiti.engine.repository.ProcessDefinitionQuery;
import org.activiti.engine.task.Task;
import org.activiti.engine.task.TaskQuery;
import org.ow2.petals.activitibpmn.incoming.integration.exception.OperationInitializationException;
import org.ow2.petals.components.activiti.generic._1.GetTasks;
import org.ow2.petals.components.activiti.generic._1.GetTasksResponse;
import org.ow2.petals.components.activiti.generic._1.InvalidRequest;
import org.ow2.petals.components.activiti.generic._1.Tasks;

/**
 * The integration operation to search tasks
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class GetTasksOperation extends AbstractOperation<GetTasks, GetTasksResponse> {

    public static final URI FAULT_ACTOR = URI.create("http://petals.ow2.org/components/activiti/generic/1.0/GetTasks");

    /**
     * The Activiti task service
     */
    private final TaskService taskService;

    /**
     * The Activiti repository service
     */
    private final RepositoryService repositoryService;

    public GetTasksOperation(final TaskService taskService, final RepositoryService repositoryService, final Logger log)
            throws OperationInitializationException {
        super(ITG_OP_GETTASKS, FAULT_ACTOR,
                new Class[] { GetTasks.class, GetTasksResponse.class, InvalidRequest.class }, log);
        this.taskService = taskService;
        this.repositoryService = repositoryService;
    }

    @Override
    public GetTasksResponse doExecute(final GetTasks incomingObject) {

        final TaskQuery taskQuery = this.taskService.createTaskQuery();

        // By default, we search active tasks
        final Boolean isActive = incomingObject.isActive();
        if (isActive != null && !isActive.booleanValue()) {
            taskQuery.suspended();
        } else {
            taskQuery.active();
        }

        final String assignee = incomingObject.getAssignee();
        if (assignee != null && !assignee.isEmpty()) {
            taskQuery.taskCandidateUser(assignee);
        }

        final String processInstanceId = incomingObject.getProcessInstanceIdentifier();
        if (processInstanceId != null && !processInstanceId.isEmpty()) {
            taskQuery.processInstanceId(processInstanceId);
        }
        
        final String processDefinitionId = incomingObject.getProcessDefinitionIdentifier();
        if (processDefinitionId != null && !processDefinitionId.isEmpty()) {
            taskQuery.processDefinitionKey(processDefinitionId);
        }
        
        final String taskDefinitionId = incomingObject.getTaskDefinitionIdentifier();
        if (taskDefinitionId != null && !taskDefinitionId.isEmpty()) {
            taskQuery.taskDefinitionKey(taskDefinitionId);
        }

        final GetTasksResponse response = new GetTasksResponse();
        final Tasks responseTasks = new Tasks();
        response.setTasks(responseTasks);
        final List<Task> tasks = taskQuery.list();
        for (final Task task : tasks) {
            final org.ow2.petals.components.activiti.generic._1.Task responseTask = new org.ow2.petals.components.activiti.generic._1.Task();
            responseTask.setProcessInstanceIdentifier(task.getProcessInstanceId());
            responseTask.setTaskIdentifier(task.getTaskDefinitionKey());
            responseTasks.getTask().add(responseTask);

            final ProcessDefinitionQuery processDefQuery = this.repositoryService.createProcessDefinitionQuery()
                    .processDefinitionId(task.getProcessDefinitionId());
            final ProcessDefinition processDefinition = processDefQuery.singleResult();
            responseTask.setProcessDefinitionIdentifier(processDefinition.getKey());
        }

        return response;

    }

}

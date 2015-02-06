/**
 * Copyright (c) 2015 Linagora
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
 * along with this program/library; If not, see <http://www.gnu.org/licenses/>
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.activitibpmn.incoming.integration;

import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_OP_GETTASKS;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;

import org.activiti.engine.RepositoryService;
import org.activiti.engine.TaskService;
import org.activiti.engine.repository.ProcessDefinition;
import org.activiti.engine.repository.ProcessDefinitionQuery;
import org.activiti.engine.task.Task;
import org.activiti.engine.task.TaskQuery;
import org.apache.commons.pool.ObjectPool;
import org.apache.commons.pool.impl.GenericObjectPool;
import org.ow2.petals.activitibpmn.incoming.ActivitiService;
import org.ow2.petals.activitibpmn.incoming.integration.exception.OperationInitializationException;
import org.ow2.petals.component.framework.api.message.Exchange;
import org.ow2.petals.components.activiti.generic._1.GetTasks;
import org.ow2.petals.components.activiti.generic._1.GetTasksResponse;
import org.ow2.petals.components.activiti.generic._1.Tasks;

/**
 * The integration operation to search tasks
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class GetTasksOperation implements ActivitiService {

    /**
     * The Activiti task service
     */
    private final TaskService taskService;

    /**
     * The Activiti repository service
     */
    private final RepositoryService repositoryService;

    /**
     * A pool of marshallers for Java-->XML binding of the operation responses
     */
    private final ObjectPool marshalerPool;

    /**
     * A pool of unmarshallers for XML-->Java binding of the operation request
     */
    private final ObjectPool unmarshalerPool;

    private final Logger log;

    public GetTasksOperation(final TaskService taskService, final RepositoryService repositoryService, final Logger log)
            throws OperationInitializationException {
        this.taskService = taskService;
        this.repositoryService = repositoryService;
        this.log = log;

        try {
            final JAXBContext jaxbContext = JAXBContext.newInstance(new Class[] { GetTasks.class,
                    GetTasksResponse.class });

            this.marshalerPool = new GenericObjectPool(new MarshalerFactory(jaxbContext));
            this.unmarshalerPool = new GenericObjectPool(new UnmarshalerFactory(jaxbContext));

        } catch (final JAXBException e) {
            throw new OperationInitializationException(ITG_OP_GETTASKS, e);
        }
    }

    @Override
    public void execute(final Exchange exchange) {
        
        try {
            final Source incomingPayload = exchange.getInMessageContentAsSource();
            if (incomingPayload != null) {
                // Unmarshal incoming request
                final Unmarshaller unmarshaller = (Unmarshaller) this.unmarshalerPool.borrowObject();
                final Object incomingObject;
                try {
                    incomingObject = unmarshaller.unmarshal(incomingPayload);
                } finally {
                    this.unmarshalerPool.returnObject(unmarshaller);
                }
                if (incomingObject instanceof GetTasks) {
                    final GetTasks incomingRequest = (GetTasks) incomingObject;
                    final TaskQuery taskQuery = this.taskService.createTaskQuery();

                    if (incomingRequest.isActive()) {
                        taskQuery.active();
                    }

                    final String assignee = incomingRequest.getAssignee();
                    if (assignee != null && !assignee.isEmpty()) {
                        taskQuery.taskCandidateOrAssigned(assignee);
                    }

                    final GetTasksResponse response = new GetTasksResponse();
                    final Tasks responseTasks = new Tasks();
                    response.setTasks(responseTasks);
                    final List<Task> tasks = taskQuery.list();
                    for (final Task task : tasks) {
                        final org.ow2.petals.components.activiti.generic._1.Task responseTask = new org.ow2.petals.components.activiti.generic._1.Task();
                        responseTask.setAssignee(task.getAssignee());
                        responseTask.setProcessInstanceIdentifier(task.getProcessInstanceId());
                        responseTask.setTaskIdentifier(task.getTaskDefinitionKey());
                        responseTasks.getTask().add(responseTask);

                        final ProcessDefinitionQuery processDefQuery = this.repositoryService
                                .createProcessDefinitionQuery().processDefinitionId(task.getProcessDefinitionId());
                        final ProcessDefinition processDefinition = processDefQuery.singleResult();
                        responseTask.setProcessDefinitionIdentifier(processDefinition.getKey());
                    }

                    // TODO: Avoid to use ByteArray(In|Out)put stream, try to pipe streams because of memory problem
                    // with big payloads
                    final ByteArrayOutputStream baos = new ByteArrayOutputStream();
                    try {
                        final Marshaller marshaller = (Marshaller) this.marshalerPool.borrowObject();
                        try {
                            marshaller.marshal(response, baos);
                        } finally {
                            this.marshalerPool.returnObject(marshaller);
                        }
                    } finally {
                        baos.close();
                    }

                    final ByteArrayInputStream bais = new ByteArrayInputStream(baos.toByteArray());
                    try {
                        exchange.setOutMessageContent(new StreamSource(bais));
                    } finally {
                        bais.close();
                    }

                } else {
                    // TODO: manage this error case: returns a fault 'InvalidRequest'
                }
            } else {
                // TODO: manage this error case: returns a fault 'InvalidRequest'
            }
        } catch (final Exception e) {
            this.log.log(Level.SEVERE, "Exchange " + exchange.getExchangeId() + " encountered a problem.", e);
            exchange.setError(e);
        }

    }

    @Override
    public void log(final Logger logger, final Level logLevel) {
        // NOP: We have nothing to log
    }

}

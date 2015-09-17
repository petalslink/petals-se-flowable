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

import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_OP_GETPROCESSINSTANCES;

import java.net.URI;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;

import org.activiti.engine.HistoryService;
import org.activiti.engine.RepositoryService;
import org.activiti.engine.RuntimeService;
import org.activiti.engine.history.HistoricProcessInstance;
import org.activiti.engine.history.HistoricProcessInstanceQuery;
import org.activiti.engine.repository.ProcessDefinition;
import org.activiti.engine.repository.ProcessDefinitionQuery;
import org.activiti.engine.runtime.ProcessInstance;
import org.activiti.engine.runtime.ProcessInstanceQuery;
import org.ow2.petals.activitibpmn.incoming.integration.exception.OperationInitializationException;
import org.ow2.petals.components.activiti.generic._1.GetProcessInstances;
import org.ow2.petals.components.activiti.generic._1.GetProcessInstancesResponse;
import org.ow2.petals.components.activiti.generic._1.InvalidRequest;
import org.ow2.petals.components.activiti.generic._1.ProcessInstanceState;
import org.ow2.petals.components.activiti.generic._1.ProcessInstances;
import org.ow2.petals.components.activiti.generic._1.Variable;
import org.ow2.petals.components.activiti.generic._1.Variables;

/**
 * The integration operation to search process instances
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class GetProcessInstancesOperation extends AbstractOperation<GetProcessInstances, GetProcessInstancesResponse> {

    public static final URI FAULT_ACTOR = URI
            .create("http://petals.ow2.org/components/activiti/generic/1.0/GetProcessInstances");

    /**
     * The Activiti task service
     */
    private final RuntimeService runtimeService;

    /**
     * The Activiti history service
     */
    private final HistoryService historyService;

    /**
     * The Activiti repository service
     */
    private final RepositoryService repositoryService;

    public GetProcessInstancesOperation(final RuntimeService runtimeService, final HistoryService historyService,
            final RepositoryService repositoryService, final Logger log) throws OperationInitializationException {
        super(ITG_OP_GETPROCESSINSTANCES, FAULT_ACTOR, new Class[] { GetProcessInstances.class,
                GetProcessInstancesResponse.class, InvalidRequest.class }, log);
        this.runtimeService = runtimeService;
        this.historyService = historyService;
        this.repositoryService = repositoryService;
    }

    @Override
    public GetProcessInstancesResponse doExecute(final GetProcessInstances incomingObject) {

        // By default, we search active process instance
        final ProcessInstanceState state = incomingObject.getState();
        if (state == null || state == ProcessInstanceState.ACTIVE) {
            return this.searchProcessInstances(incomingObject, ProcessInstanceState.ACTIVE);
        } else if (state != null && state == ProcessInstanceState.SUSPENDED) {
            return this.searchProcessInstances(incomingObject, ProcessInstanceState.SUSPENDED);
        } else {
            // state == ProcessInstanceState.FINISHED
            return this.searchHistoricProcessInstances(incomingObject, ProcessInstanceState.FINISHED);
        }
    }

    /**
     * Search process instances that are not ended (in state 'active' or 'suspended').
     */
    private GetProcessInstancesResponse searchProcessInstances(final GetProcessInstances incomingObject,
            final ProcessInstanceState state) {

        final ProcessInstanceQuery processInstanceQuery = this.runtimeService.createProcessInstanceQuery();

        if (state == ProcessInstanceState.ACTIVE) {
            processInstanceQuery.active();
        } else {
            // state == ProcessInstanceState.SUSPENDED
            processInstanceQuery.suspended();
        }

        final String processDefinitionId = incomingObject.getProcessDefinitionIdentifier();
        if (processDefinitionId != null && !processDefinitionId.isEmpty()) {
            processInstanceQuery.processDefinitionKey(processDefinitionId);
        }

        final String processInstanceId = incomingObject.getProcessInstanceIdentifier();
        if (processInstanceId != null && !processInstanceId.isEmpty()) {
            processInstanceQuery.processInstanceId(processInstanceId);
        }

        processInstanceQuery.includeProcessVariables();

        final GetProcessInstancesResponse response = new GetProcessInstancesResponse();
        final ProcessInstances responseProcessInstances = new ProcessInstances();
        response.setProcessInstances(responseProcessInstances);
        final List<ProcessInstance> processInstances = processInstanceQuery.list();
        for (final ProcessInstance processInstance : processInstances) {
            final org.ow2.petals.components.activiti.generic._1.ProcessInstance responseProcessInstance = new org.ow2.petals.components.activiti.generic._1.ProcessInstance();
            responseProcessInstance.setProcessInstanceIdentifier(processInstance.getProcessInstanceId());
            responseProcessInstances.getProcessInstance().add(responseProcessInstance);

            final ProcessDefinitionQuery processDefQuery = this.repositoryService.createProcessDefinitionQuery()
                    .processDefinitionId(processInstance.getProcessDefinitionId());
            final ProcessDefinition processDefinition = processDefQuery.singleResult();
            responseProcessInstance.setProcessDefinitionIdentifier(processDefinition.getKey());

            final Variables responseProcessVariables = new Variables();
            responseProcessInstance.setVariables(responseProcessVariables);
            for (final Entry<String, Object> variable : processInstance.getProcessVariables().entrySet()) {

                final Variable responseProcessVariable = new Variable();
                final Object variableValue = variable.getValue();
                responseProcessVariable.setName(variable.getKey());
                if (variableValue instanceof Date) {
                    final GregorianCalendar calendar = new GregorianCalendar();
                    calendar.setTime((Date) variableValue);
                    try {
                        responseProcessVariable.setValue(DatatypeFactory.newInstance()
                                .newXMLGregorianCalendar(calendar).toXMLFormat());
                    } catch (final DatatypeConfigurationException e) {
                        this.log.log(Level.WARNING, "Error converting a date to XML.", e);
                    }

                } else {
                    responseProcessVariable.setValue(variableValue != null ? variableValue.toString() : "");
                }
                responseProcessVariables.getVariable().add(responseProcessVariable);
            }
        }

        return response;
    }

    /**
     * Search process instances that are ended (in state 'finished').
     */
    private GetProcessInstancesResponse searchHistoricProcessInstances(final GetProcessInstances incomingObject,
            final ProcessInstanceState state) {

        final HistoricProcessInstanceQuery processInstanceQuery = this.historyService
                .createHistoricProcessInstanceQuery();

        processInstanceQuery.finished();

        final String processDefinitionId = incomingObject.getProcessDefinitionIdentifier();
        if (processDefinitionId != null && !processDefinitionId.isEmpty()) {
            processInstanceQuery.processDefinitionKey(processDefinitionId);
        }

        final String processInstanceId = incomingObject.getProcessInstanceIdentifier();
        if (processInstanceId != null && !processInstanceId.isEmpty()) {
            processInstanceQuery.processInstanceId(processInstanceId);
        }

        processInstanceQuery.includeProcessVariables();

        final GetProcessInstancesResponse response = new GetProcessInstancesResponse();
        final ProcessInstances responseProcessInstances = new ProcessInstances();
        response.setProcessInstances(responseProcessInstances);
        final List<HistoricProcessInstance> processInstances = processInstanceQuery.list();
        for (final HistoricProcessInstance processInstance : processInstances) {
            final org.ow2.petals.components.activiti.generic._1.ProcessInstance responseProcessInstance = new org.ow2.petals.components.activiti.generic._1.ProcessInstance();
            responseProcessInstance.setProcessInstanceIdentifier(processInstance.getId());
            responseProcessInstances.getProcessInstance().add(responseProcessInstance);

            final ProcessDefinitionQuery processDefQuery = this.repositoryService.createProcessDefinitionQuery()
                    .processDefinitionId(processInstance.getProcessDefinitionId());
            final ProcessDefinition processDefinition = processDefQuery.singleResult();
            responseProcessInstance.setProcessDefinitionIdentifier(processDefinition.getKey());

            final Variables responseProcessVariables = new Variables();
            responseProcessInstance.setVariables(responseProcessVariables);
            for (final Entry<String, Object> variable : processInstance.getProcessVariables().entrySet()) {

                final Variable responseProcessVariable = new Variable();
                final Object variableValue = variable.getValue();
                responseProcessVariable.setName(variable.getKey());
                if (variableValue instanceof Date) {
                    final GregorianCalendar calendar = new GregorianCalendar();
                    calendar.setTime((Date) variableValue);
                    try {
                        responseProcessVariable.setValue(DatatypeFactory.newInstance()
                                .newXMLGregorianCalendar(calendar).toXMLFormat());
                    } catch (final DatatypeConfigurationException e) {
                        this.log.log(Level.WARNING, "Error converting a date to XML.", e);
                    }

                } else {
                    responseProcessVariable.setValue(variableValue != null ? variableValue.toString() : "");
                }
                responseProcessVariables.getVariable().add(responseProcessVariable);
            }
        }

        return response;
    }

}

/**
 * Copyright (c) 2018-2022 Linagora
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
package org.ow2.petals.flowable.admin;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.flowable.engine.repository.Deployment;
import org.flowable.engine.runtime.ProcessInstance;
import org.flowable.task.api.Task;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.ow2.petals.basisapi.exception.PetalsException;
import org.ow2.petals.flowable.AbstractVacationProcessTestEnvironment;
import org.ow2.petals.flowable.junit.FlowableClient;
import org.ow2.petals.se.flowable.clientserver.api.admin.exception.ProcessDefinitionNotFoundException;
import org.ow2.petals.se.flowable.clientserver.api.admin.exception.ProcessInstanceExistForDefinitionException;

/**
 * Unit test of {@link AdminOperations}
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class AdminOperationsTest {

    public static final String BPMN_PROCESS_DEFINITION_KEY = "adminTestProcess";

    @Rule
    public FlowableClient flowableClient = new FlowableClient();

    @Before
    public void deployProcessDefinition() {
        this.flowableClient.getRepositoryService().createDeployment().addClasspathResource("adminTestProcess.bpmn")
                .deploy();
    }

    @After
    public void forceProcessDefUndeploiement() {

        // Force process definition undeployment is not undeploy by unit test
        final Deployment deployment = this.flowableClient.getRepositoryService().createDeploymentQuery()
                .processDefinitionKey(BPMN_PROCESS_DEFINITION_KEY).singleResult();
        if (deployment != null) {
            this.flowableClient.getRepositoryService().deleteDeployment(deployment.getId(), true);
        }
    }

    @Test
    public void undeployProcDef() throws PetalsException {
        AdminOperations.undeployProcessDefinition(BPMN_PROCESS_DEFINITION_KEY, 1,
                this.flowableClient.getProcessEngine());
    }

    @Test(expected = ProcessDefinitionNotFoundException.class)
    public void undeployProcDefNotFound() throws PetalsException {
        AdminOperations.undeployProcessDefinition("unexisting-process-definition", 1,
                this.flowableClient.getProcessEngine());
    }

    @Test(expected = ProcessInstanceExistForDefinitionException.class)
    public void undeployProcDefWithExistingActiveProcInst() throws PetalsException {

        // Start a process instance
        final Map<String, Object> variables = new HashMap<>();
        variables.put("numberOfDays", 10);
        variables.put("startDate", new Date());
        variables.put("vacationMotivation", "Holidays");
        final ProcessInstance procInst;
        this.flowableClient.getIdentityService()
                .setAuthenticatedUserId(AbstractVacationProcessTestEnvironment.BPMN_USER_DEMANDEUR);
        try {
            procInst = this.flowableClient.getRuntimeService().startProcessInstanceByKey(BPMN_PROCESS_DEFINITION_KEY,
                    variables);
        } finally {
            this.flowableClient.getIdentityService().setAuthenticatedUserId(null);
        }

        assertNull(this.flowableClient.getHistoryService().createHistoricProcessInstanceQuery().finished()
                .processInstanceId(procInst.getId()).singleResult());

        // Try to undeploy process definition
        AdminOperations.undeployProcessDefinition(BPMN_PROCESS_DEFINITION_KEY, 1,
                this.flowableClient.getProcessEngine());
    }

    @Test(expected = ProcessInstanceExistForDefinitionException.class)
    public void undeployProcDefWithExistingEndedProcInst() throws PetalsException {

        // Start a process instance
        final Map<String, Object> variables = new HashMap<>();
        variables.put("numberOfDays", 10);
        variables.put("startDate", new Date());
        variables.put("vacationMotivation", "Holidays");
        final ProcessInstance procInst;
        this.flowableClient.getIdentityService()
                .setAuthenticatedUserId(AbstractVacationProcessTestEnvironment.BPMN_USER_DEMANDEUR);
        try {
            procInst = this.flowableClient.getRuntimeService().startProcessInstanceByKey(BPMN_PROCESS_DEFINITION_KEY,
                    variables);
        } finally {
            this.flowableClient.getIdentityService().setAuthenticatedUserId(null);
        }

        // Validate user task
        final Task task = this.flowableClient.getTaskService().createTaskQuery().active()
                .processInstanceId(procInst.getId()).singleResult();
        variables.clear();
        variables.put("vacationApproved", "true");
        this.flowableClient.getIdentityService()
                .setAuthenticatedUserId(AbstractVacationProcessTestEnvironment.BPMN_USER_VALIDEUR);
        try {
            this.flowableClient.getTaskService().complete(task.getId(), variables);
        } finally {
            this.flowableClient.getIdentityService().setAuthenticatedUserId(null);
        }

        assertNotNull(this.flowableClient.getHistoryService().createHistoricProcessInstanceQuery().finished()
                .processInstanceId(procInst.getId()).singleResult());

        // Try to undeploy process definition
        AdminOperations.undeployProcessDefinition(BPMN_PROCESS_DEFINITION_KEY, 1,
                this.flowableClient.getProcessEngine());
    }
}

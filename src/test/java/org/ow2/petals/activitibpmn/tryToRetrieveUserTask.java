/**
 * Copyright (c) 2014-2016 Linagora
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
package org.ow2.petals.activitibpmn;

import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_OP_ACTIVATEPROCESSINSTANCES;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_OP_GETPROCESSINSTANCES;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_OP_GETTASKS;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_OP_SUSPENDPROCESSINSTANCES;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_PROCESSINSTANCES_PORT_TYPE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_PROCESSINSTANCES_SERVICE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_TASK_PORT_TYPE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_TASK_SERVICE;

import java.io.IOException;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.logging.LogRecord;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.xml.bind.DatatypeConverter;
import javax.xml.bind.JAXBException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.transform.Source;

import org.junit.Test;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation;
import org.ow2.petals.activitibpmn.incoming.operation.exception.NoProcessInstanceIdValueException;
import org.ow2.petals.activitibpmn.incoming.operation.exception.NoUserIdValueException;
import org.ow2.petals.activitibpmn.monitoring.ActivitiActivityFlowStepData;
import org.ow2.petals.activitibpmn.monitoring.ProcessInstanceFlowStepBeginLogData;
import org.ow2.petals.activitibpmn.monitoring.UserTaskFlowStepBeginLogData;
import org.ow2.petals.commons.log.FlowLogData;
import org.ow2.petals.commons.log.Level;
import org.ow2.petals.component.framework.junit.Message;
import org.ow2.petals.component.framework.junit.RequestMessage;
import org.ow2.petals.component.framework.junit.ResponseMessage;
import org.ow2.petals.component.framework.junit.StatusMessage;
import org.ow2.petals.component.framework.junit.helpers.ServiceProviderImplementation;
import org.ow2.petals.component.framework.junit.impl.message.RequestToProviderMessage;
import org.ow2.petals.component.framework.junit.impl.message.ResponseToConsumerMessage;
import org.ow2.petals.components.activiti.generic._1.ActivateProcessInstances;
import org.ow2.petals.components.activiti.generic._1.ActivateProcessInstancesResponse;
import org.ow2.petals.components.activiti.generic._1.ActivationResult;
import org.ow2.petals.components.activiti.generic._1.AdjournmentResult;
import org.ow2.petals.components.activiti.generic._1.GetProcessInstances;
import org.ow2.petals.components.activiti.generic._1.GetProcessInstancesResponse;
import org.ow2.petals.components.activiti.generic._1.GetTasks;
import org.ow2.petals.components.activiti.generic._1.GetTasksResponse;
import org.ow2.petals.components.activiti.generic._1.ProcessInstance;
import org.ow2.petals.components.activiti.generic._1.ProcessInstanceState;
import org.ow2.petals.components.activiti.generic._1.SuspendProcessInstances;
import org.ow2.petals.components.activiti.generic._1.SuspendProcessInstancesResponse;
import org.ow2.petals.components.activiti.generic._1.Task;
import org.ow2.petals.components.activiti.generic._1.Variable;
import org.ow2.petals.samples.se_bpmn.archivageservice.Archiver;
import org.ow2.petals.samples.se_bpmn.archivageservice.ArchiverResponse;
import org.ow2.petals.samples.se_bpmn.vacationservice.AckResponse;
import org.ow2.petals.samples.se_bpmn.vacationservice.Demande;
import org.ow2.petals.samples.se_bpmn.vacationservice.DemandeDejaValidee;
import org.ow2.petals.samples.se_bpmn.vacationservice.JiraPETALSSEACTIVITI4;
import org.ow2.petals.samples.se_bpmn.vacationservice.Numero;
import org.ow2.petals.samples.se_bpmn.vacationservice.NumeroDemandeInconnu;
import org.ow2.petals.samples.se_bpmn.vacationservice.Validation;
import org.ow2.petals.samples.se_bpmn.vacationservice.XslParameter;

import com.ebmwebsourcing.easycommons.xml.SourceHelper;

/**
 * Unit tests about request processing of BPMN services, with a component configured with default values
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class tryToRetrieveUserTask extends AbstractComponentTest {

    /**
     * <p>
     * Check the message processing where:
     * <ol>
     * <li>a first valid request is sent to create a new process instance,</li>
     * <li>a 2nd valid request is sent to complete the waiting user task,</li>
     * <li>a 3rd valid request is sent to complete again the user task already completed by the 2nd request.</li>
     * </ol>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>on the first request:
     * <ul>
     * <li>the process instance is correctly created in the Activiti engine,</li>
     * <li>the 1st user task is correctly assigned,</li>
     * <li>the service reply is as expected,</li>
     * </ul>
     * </li>
     * <li>on the 2nd request,:
     * <ul>
     * <li>the process instance is correctly finished in the Activiti engine,</li>
     * <li>the 1st user task is completed by the right assignee,</li>
     * <li>the service reply is as expected,</li>
     * <li>service tasks invoked Petals services</li>
     * </ul>
     * </li>
     * <li>on the 3rd request, the right business fault associated to a user task already completed is returned because
     * the process instance is finished.</li>
     * </ul>
     * </p>
     */
    @Test
    public void validStartEventRequest() throws Exception {

        assertTrue(this.activitiClient.getIdentityService().checkPassword(BPMN_USER_DEMANDEUR, "demandeur"));
        assertEquals(BPMN_USER_DEMANDEUR,
                this.activitiClient.getIdentityService().createUserQuery().memberOfGroup("employees").singleResult()
                        .getId());
        assertEquals(BPMN_USER_VALIDEUR,
                this.activitiClient.getIdentityService().createUserQuery().memberOfGroup("management").singleResult()
                        .getId());
        assertEquals("employees",
                this.activitiClient.getIdentityService().createGroupQuery().groupMember(BPMN_USER_DEMANDEUR)
                        .singleResult().getId());
        assertEquals("management",
                this.activitiClient.getIdentityService().createGroupQuery().groupMember(BPMN_USER_VALIDEUR)
                        .singleResult().getId());

        // --------------------------------------------------------
        // ---- Create a new instance of the process definition
        // --------------------------------------------------------
        final Demande request_1 = new Demande();
        request_1.setDemandeur(BPMN_USER_DEMANDEUR);
        final int numberOfDays = 10;
        request_1.setNbJourDde(numberOfDays);
        final GregorianCalendar now = new GregorianCalendar();
        final GregorianCalendar startDate = new GregorianCalendar(now.get(GregorianCalendar.YEAR),
                now.get(GregorianCalendar.MONTH), now.get(GregorianCalendar.DAY_OF_MONTH));
        request_1.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(startDate));
        final String motivation = "hollidays";
        request_1.setMotifDde(motivation);

        // Send the 1st valid request for start event 'request
        final RequestToProviderMessage request = new RequestToProviderMessage(
                COMPONENT_UNDER_TEST, VALID_SU, OPERATION_DEMANDERCONGES,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), this.toByteArray(request_1));

        // Assert the response of the 1st valid request
        final ResponseMessage responseMsg_1 = COMPONENT.sendAndGetResponse(request);

        // Check the reply
        final Source fault_1 = responseMsg_1.getFault();
        assertNull("Unexpected fault", (fault_1 == null ? null : SourceHelper.toString(fault_1)));
        assertNotNull("No XML payload in response", responseMsg_1.getPayload());
        final Object responseObj_1 = UNMARSHALLER.unmarshal(responseMsg_1.getPayload());
        assertTrue(responseObj_1 instanceof Numero);
        final Numero response_1 = (Numero) responseObj_1;
        assertNotNull(response_1.getNumeroDde());
        {
            assertEquals(5, response_1.getXslParameter().size());
            boolean userFound = false;
            boolean employeeFound = false;
            boolean numberOfDaysFound = false;
            boolean startDateFound = false;
            boolean motivationFound = false;
            for (final XslParameter xslParameter : response_1.getXslParameter()) {
                if ("user-id".equals(xslParameter.getName().toString())) {
                    userFound = true;
                    assertEquals(BPMN_USER_DEMANDEUR, xslParameter.getValue());
                } else if ("employeeName".equals(xslParameter.getName().toString())) {
                    employeeFound = true;
                    assertEquals(BPMN_USER_DEMANDEUR, xslParameter.getValue());
                } else if ("numberOfDays".equals(xslParameter.getName().toString())) {
                    numberOfDaysFound = true;
                    assertEquals(numberOfDays, Integer.parseInt(xslParameter.getValue()));
                } else if ("startDate".equals(xslParameter.getName().toString())) {
                    startDateFound = true;
                    // Don't use 'assertEquals' because time zones can be different
                    assertTrue(startDate.compareTo(DatatypeConverter.parseDate(xslParameter.getValue())) == 0);
                } else if ("vacationMotivation".equals(xslParameter.getName().toString())) {
                    motivationFound = true;
                    assertEquals(motivation, xslParameter.getValue());
                } else {
                    fail("Unexpected xsl parameter: " + xslParameter.getName().toString());
                }
            }
            assertTrue(userFound);
            assertTrue(employeeFound);
            assertTrue(numberOfDaysFound);
            assertTrue(startDateFound);
            assertTrue(motivationFound);
        }
        assertProcessInstancePending(response_1.getNumeroDde(), BPMN_PROCESS_DEFINITION_KEY);
        assertCurrentUserTask(response_1.getNumeroDde(), BPMN_PROCESS_1ST_USER_TASK_KEY, BPMN_USER_VALIDEUR);

        // Check MONIT traces
        final List<LogRecord> monitLogs_1 = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(4, monitLogs_1.size());
        final FlowLogData initialInteractionRequestFlowLogData = assertMonitProviderBeginLog(VACATION_INTERFACE,
                VACATION_SERVICE, VACATION_ENDPOINT, OPERATION_DEMANDERCONGES, monitLogs_1.get(0));
        final FlowLogData processStartedBeginFlowLogData = assertMonitConsumerExtBeginLog(monitLogs_1.get(1));
        assertEquals(initialInteractionRequestFlowLogData.get(FlowLogData.FLOW_INSTANCE_ID_PROPERTY_NAME),
                processStartedBeginFlowLogData.get(ActivitiActivityFlowStepData.CORRELATED_FLOW_INSTANCE_ID_KEY));
        assertEquals(initialInteractionRequestFlowLogData.get(FlowLogData.FLOW_STEP_ID_PROPERTY_NAME),
                processStartedBeginFlowLogData.get(ActivitiActivityFlowStepData.CORRELATED_FLOW_STEP_ID_KEY));
        assertEquals("vacationRequest",
                processStartedBeginFlowLogData.get(ProcessInstanceFlowStepBeginLogData.PROCESS_DEFINITION_KEY));
        assertEquals(response_1.getNumeroDde(),
                processStartedBeginFlowLogData.get(ProcessInstanceFlowStepBeginLogData.PROCESS_INSTANCE_ID_KEY));
        final FlowLogData userTaskHandleRequestBeginFlowLogData = assertMonitProviderBeginLog(
                processStartedBeginFlowLogData, null, null, null, null, monitLogs_1.get(2));
        assertEquals("handleRequest",
                userTaskHandleRequestBeginFlowLogData.get(UserTaskFlowStepBeginLogData.TASK_DEFINITION_KEY));
        assertNotNull(userTaskHandleRequestBeginFlowLogData.get(UserTaskFlowStepBeginLogData.TASK_INSTANCE_ID_KEY));
        assertMonitProviderEndLog(initialInteractionRequestFlowLogData, monitLogs_1.get(3));

        // Check that internal process variables are correctly set
        assertEquals(
                processStartedBeginFlowLogData.get(FlowLogData.FLOW_INSTANCE_ID_PROPERTY_NAME),
                this.activitiClient.getRuntimeService().getVariable(response_1.getNumeroDde(),
                        ActivitiSEConstants.Activiti.VAR_PETALS_FLOW_INSTANCE_ID));
        assertEquals(
                processStartedBeginFlowLogData.get(FlowLogData.FLOW_STEP_ID_PROPERTY_NAME),
                this.activitiClient.getRuntimeService().getVariable(response_1.getNumeroDde(),
                        ActivitiSEConstants.Activiti.VAR_PETALS_FLOW_STEP_ID));

        this.tryToRetrieveProcessInstance(response_1.getNumeroDde(), startDate, motivation, numberOfDays,
                ProcessInstanceState.ACTIVE,
                processStartedBeginFlowLogData, true);
        this.tryToRetrieveProcessInstance(response_1.getNumeroDde(), startDate, motivation, numberOfDays,
                ProcessInstanceState.SUSPENDED, processStartedBeginFlowLogData, false);
        this.tryToRetrieveProcessInstance(response_1.getNumeroDde(), startDate, motivation, numberOfDays,
                ProcessInstanceState.FINISHED,
                processStartedBeginFlowLogData, false);

        this.retrieveUserTask(BPMN_USER_VALIDEUR, response_1.getNumeroDde(), true, processStartedBeginFlowLogData, true);
        this.retrieveUserTask(BPMN_USER_VALIDEUR, response_1.getNumeroDde(), false, processStartedBeginFlowLogData,
                false);

        this.tryToSuspendProcessInstance(response_1.getNumeroDde(), processStartedBeginFlowLogData,
                AdjournmentResult.SUSPENDED);
        this.tryToSuspendProcessInstance(response_1.getNumeroDde(), processStartedBeginFlowLogData,
                AdjournmentResult.ALREADY_SUSPENDED);
        this.tryToSuspendProcessInstance("not-found", processStartedBeginFlowLogData, AdjournmentResult.NOT_FOUND);

        this.tryToRetrieveProcessInstance(response_1.getNumeroDde(), startDate, motivation, numberOfDays,
                ProcessInstanceState.ACTIVE, processStartedBeginFlowLogData, false);
        this.tryToRetrieveProcessInstance(response_1.getNumeroDde(), startDate, motivation, numberOfDays,
                ProcessInstanceState.SUSPENDED,
                processStartedBeginFlowLogData, true);
        this.tryToRetrieveProcessInstance(response_1.getNumeroDde(), startDate, motivation, numberOfDays,
                ProcessInstanceState.FINISHED, processStartedBeginFlowLogData, false);

        this.retrieveUserTask(BPMN_USER_VALIDEUR, response_1.getNumeroDde(), false, processStartedBeginFlowLogData,
                true);
        this.retrieveUserTask(BPMN_USER_VALIDEUR, response_1.getNumeroDde(), true, processStartedBeginFlowLogData,
                false);

        this.tryToActivateProcessInstance(response_1.getNumeroDde(), processStartedBeginFlowLogData,
                ActivationResult.ACTIVATED);
        this.tryToActivateProcessInstance(response_1.getNumeroDde(), processStartedBeginFlowLogData,
                ActivationResult.ALREADY_ACTIVATED);
        this.tryToActivateProcessInstance("not-found", processStartedBeginFlowLogData, ActivationResult.NOT_FOUND);

        this.tryToRetrieveProcessInstance(response_1.getNumeroDde(), startDate, motivation, numberOfDays,
                ProcessInstanceState.ACTIVE, processStartedBeginFlowLogData, true);

        // ------------------------------------------------------------------
        // ---- Complete the first user task (validate the vacation request)
        // ------------------------------------------------------------------
        final Validation request_2 = new Validation();
        request_2.setValideur(BPMN_USER_VALIDEUR);
        request_2.setNumeroDde(response_1.getNumeroDde());
        request_2.setApprobation(Boolean.TRUE.toString());

        // Send the 2nd valid request
        IN_MEMORY_LOG_HANDLER.clear();
        
        final ServiceProviderImplementation service = new ServiceProviderImplementation() {
            private MessageExchange archiveMessageExchange_1;

            @Override
            public Message provides(final RequestMessage archiveRequestMsg_1) throws Exception {
             // Assert the 1st request sent by Activiti on orchestrated service
                archiveMessageExchange_1 = archiveRequestMsg_1.getMessageExchange();
                assertNotNull(archiveMessageExchange_1);
                assertEquals(ARCHIVE_INTERFACE, archiveMessageExchange_1.getInterfaceName());
                assertEquals(ARCHIVE_SERVICE, archiveMessageExchange_1.getService());
                assertNotNull(archiveMessageExchange_1.getEndpoint());
                assertEquals(ARCHIVE_ENDPOINT, archiveMessageExchange_1.getEndpoint().getEndpointName());
                assertEquals(ARCHIVER_OPERATION, archiveMessageExchange_1.getOperation());
                assertEquals(archiveMessageExchange_1.getStatus(), ExchangeStatus.ACTIVE);
                final Object archiveRequestObj_1 = UNMARSHALLER.unmarshal(archiveRequestMsg_1
                        .getPayload());
                assertTrue(archiveRequestObj_1 instanceof Archiver);
                final Archiver archiveRequest_1 = (Archiver) archiveRequestObj_1;
                assertEquals(response_1.getNumeroDde(), archiveRequest_1.getItem());

                // Returns the reply of the service provider to the Activiti service task
                final ArchiverResponse archiverResponse_1 = new ArchiverResponse();
                archiverResponse_1.setItem("value of item");
                archiverResponse_1.setItem2("value of item2");
                return new ResponseToConsumerMessage(archiveRequestMsg_1, toByteArray(archiverResponse_1));
            }
            
            @Override
            public void handleStatus(StatusMessage statusDoneMsg_1) throws Exception {
                // Assert the status DONE on the message exchange
                assertNotNull(statusDoneMsg_1);
                // It's the same message exchange instance
                assertSame(statusDoneMsg_1.getMessageExchange(), archiveMessageExchange_1);
                assertEquals(statusDoneMsg_1.getMessageExchange().getStatus(), ExchangeStatus.DONE);
            }
        };

        final RequestToProviderMessage requestM = new RequestToProviderMessage(
                COMPONENT_UNDER_TEST, VALID_SU, OPERATION_VALIDERDEMANDE,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), this.toByteArray(request_2));

        // Assert the response of the 2nd valid request
        final ResponseMessage responseMsg_2 = COMPONENT.sendAndGetResponse(requestM, service);
        
        // Check the reply
        final Source fault_2 = responseMsg_2.getFault();
        assertNull("Unexpected fault", (fault_2 == null ? null : SourceHelper.toString(fault_2)));
        assertNotNull("No XML payload in response", responseMsg_2.getPayload());
        final Object responseObj_2 = UNMARSHALLER.unmarshal(responseMsg_2.getPayload());
        assertTrue(responseObj_2 instanceof AckResponse);
        final AckResponse response_2 = (AckResponse) responseObj_2;
        {
            assertEquals(7, response_2.getXslParameter().size());
            boolean userFound = false;
            boolean processInstanceIdFound = false;
            boolean employeeFound = false;
            boolean numberOfDaysFound = false;
            boolean startDateFound = false;
            boolean approvedFound = false;
            boolean motivationFound = false;
            for (final XslParameter xslParameter : response_2.getXslParameter()) {
                if ("process-instance-id".equals(xslParameter.getName().toString())) {
                    processInstanceIdFound = true;
                    assertEquals(response_1.getNumeroDde(), xslParameter.getValue());
                } else if ("user-id".equals(xslParameter.getName().toString())) {
                    userFound = true;
                    assertEquals(BPMN_USER_VALIDEUR, xslParameter.getValue());
                } else if ("employeeName".equals(xslParameter.getName().toString())) {
                    employeeFound = true;
                    assertEquals(BPMN_USER_DEMANDEUR, xslParameter.getValue());
                } else if ("numberOfDays".equals(xslParameter.getName().toString())) {
                    numberOfDaysFound = true;
                    assertEquals(numberOfDays, Integer.parseInt(xslParameter.getValue()));
                } else if ("startDate".equals(xslParameter.getName().toString())) {
                    startDateFound = true;
                    // Don't use 'assertEquals' because time zones can be different
                    assertTrue(startDate.compareTo(DatatypeConverter.parseDate(xslParameter.getValue())) == 0);
                } else if ("vacationApproved".equals(xslParameter.getName().toString())) {
                    approvedFound = true;
                    assertEquals(Boolean.TRUE.toString(), xslParameter.getValue());
                } else if ("vacationMotivation".equals(xslParameter.getName().toString())) {
                    motivationFound = true;
                    assertEquals(motivation, xslParameter.getValue());
                } else {
                    fail("Unexpected xsl parameter: " + xslParameter.getName().toString());
                }
            }
            assertTrue(userFound);
            assertTrue(processInstanceIdFound);
            assertTrue(employeeFound);
            assertTrue(numberOfDaysFound);
            assertTrue(startDateFound);
            assertTrue(approvedFound);
            assertTrue(motivationFound);
        }

        COMPONENT.sendDoneStatus(responseMsg_2, service);

        // Wait the end of a service task (a service task is executed asynchronously by SE Activiti, see
        // PETALSSEACTIVITI-4)
        this.waitEndOfServiceTask(response_1.getNumeroDde(), "archiverLaDemandeService");
        
        // Check MONIT traces
        final List<LogRecord> monitLogs_2 = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(6, monitLogs_2.size());
        final FlowLogData completionTaskInteractionRequestFlowLogData = assertMonitProviderBeginLog(VACATION_INTERFACE,
                VACATION_SERVICE, VACATION_ENDPOINT, OPERATION_VALIDERDEMANDE, monitLogs_2.get(0));
        final FlowLogData userTaskHandleRequestEndFlowLogData = assertMonitProviderEndLog(
                userTaskHandleRequestBeginFlowLogData, monitLogs_2.get(1));
        assertEquals(
                userTaskHandleRequestEndFlowLogData.get(ActivitiActivityFlowStepData.CORRELATED_FLOW_INSTANCE_ID_KEY),
                completionTaskInteractionRequestFlowLogData.get(FlowLogData.FLOW_INSTANCE_ID_PROPERTY_NAME));
        assertEquals(userTaskHandleRequestEndFlowLogData.get(ActivitiActivityFlowStepData.CORRELATED_FLOW_STEP_ID_KEY),
                completionTaskInteractionRequestFlowLogData.get(FlowLogData.FLOW_STEP_ID_PROPERTY_NAME));
        assertMonitProviderEndLog(completionTaskInteractionRequestFlowLogData, monitLogs_2.get(2));
        // TODO: Should we have MONIT traces about the service call on the consumer side ?
        // Check the MONIT traces of the invoked services: the service is responsible to generate them, not the SE
        // Activiti.
        final FlowLogData serviceTaskFlowLogData = assertMonitProviderBeginLog(processStartedBeginFlowLogData,
                ARCHIVE_INTERFACE, ARCHIVE_SERVICE, ARCHIVE_ENDPOINT, ARCHIVER_OPERATION, monitLogs_2.get(3));
        assertMonitProviderEndLog(serviceTaskFlowLogData, monitLogs_2.get(4));
        assertMonitConsumerExtEndLog(processStartedBeginFlowLogData, monitLogs_2.get(5));

        assertProcessInstanceFinished(response_1.getNumeroDde());
        assertUserTaskEnded(response_1.getNumeroDde(), BPMN_PROCESS_1ST_USER_TASK_KEY, BPMN_USER_VALIDEUR);

        this.tryToRetrieveProcessInstance(response_1.getNumeroDde(), startDate, motivation, numberOfDays,
                ProcessInstanceState.ACTIVE, processStartedBeginFlowLogData, false);
        this.tryToRetrieveProcessInstance(response_1.getNumeroDde(), startDate, motivation, numberOfDays,
                ProcessInstanceState.SUSPENDED, processStartedBeginFlowLogData, false);
        this.tryToRetrieveProcessInstance(response_1.getNumeroDde(), startDate, motivation, numberOfDays,
                ProcessInstanceState.FINISHED,
                processStartedBeginFlowLogData, true);

        // --------------------------------------------------------
        // ---- Try to complete AGAIN the first user task
        // --------------------------------------------------------
        IN_MEMORY_LOG_HANDLER.clear();

        final RequestToProviderMessage requestM2 = new RequestToProviderMessage(
                COMPONENT_UNDER_TEST, VALID_SU, OPERATION_VALIDERDEMANDE,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(request_2));

        // Assert the response of the 3rd valid request
        final ResponseMessage responseMsg_3 = COMPONENT.sendAndGetResponse(requestM2);

        // Check MONIT traces
        final List<LogRecord> monitLogs_3 = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs_3.size());
        assertMonitProviderFailureLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_VALIDERDEMANDE, monitLogs_3.get(0)), monitLogs_3.get(1));

        // Check the reply
        assertNull("Out in response", responseMsg_3.getOut());
        final Source fault_3 = responseMsg_3.getFault();
        assertNotNull("No fault returns", fault_3);
        final Object responseObj_3 = UNMARSHALLER.unmarshal(fault_3);
        assertTrue(responseObj_3 instanceof DemandeDejaValidee);
        final DemandeDejaValidee response_3 = (DemandeDejaValidee) responseObj_3;
        assertEquals(response_1.getNumeroDde(), response_3.getNumeroDde());

        COMPONENT.sendDoneStatus(responseMsg_3);
    }

    /**
     * <p>
     * Check the message processing where a request is sent to create a new process instance, where:
     * <ul>
     * <li>the user identifier is missing into the incoming payload.</li>
     * </ul>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>a fault is returned to the caller</li>
     * <li>no process instance is created on the Activiti engine</li>
     * </ul>
     * </p>
     */
    @Test
    public void startEventRequest_NoUserIdValue() throws Exception {

        final int currentProcInstNb = this.getProcessInstanceNumber(BPMN_PROCESS_DEFINITION_KEY);

        // Create the 1st valid request
        final Demande request = new Demande();
        request.setNbJourDde(10);
        request.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(new GregorianCalendar()));
        request.setMotifDde("hollidays");

        // Send the request
        final RequestToProviderMessage requestM = new RequestToProviderMessage(
                COMPONENT_UNDER_TEST, VALID_SU, OPERATION_DEMANDERCONGES,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(request));

        // Assert the response of the request
        final StatusMessage responseMsg = COMPONENT.sendAndGetStatus(requestM);

        // Check MONIT traces
        final List<LogRecord> monitLogs = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs.size());
        assertMonitProviderFailureLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_DEMANDERCONGES, monitLogs.get(0)), monitLogs.get(1));

        // Check the reply
        final Exception error = responseMsg.getError();
        assertNotNull("No error returns", error);
        assertTrue("Unexpected fault", error.getCause() instanceof NoUserIdValueException);
        assertNull("XML payload in response", responseMsg.getOut());

        assertEquals(currentProcInstNb, this.getProcessInstanceNumber(BPMN_PROCESS_DEFINITION_KEY));
    }

    /**
     * <p>
     * Check the message processing where a request is sent to create a new process instance, where:
     * <ul>
     * <li>the user identifier is empty into the incoming payload.</li>
     * </ul>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>a fault is returned to the caller</li>
     * </ul>
     * </p>
     */
    @Test
    public void startEventRequest_EmptyUserIdValue() throws Exception {

        final int currentProcInstNb = this.getProcessInstanceNumber(BPMN_PROCESS_DEFINITION_KEY);

        // Create the 1st valid request
        final Demande request = new Demande();
        request.setDemandeur("");
        request.setNbJourDde(10);
        request.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(new GregorianCalendar()));
        request.setMotifDde("hollidays");

        // Send the request
        final RequestToProviderMessage requestM = new RequestToProviderMessage(
                COMPONENT_UNDER_TEST, VALID_SU, OPERATION_DEMANDERCONGES,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(request));

        // Assert the response of the request
        final StatusMessage responseMsg = COMPONENT.sendAndGetStatus(requestM);

        // Check MONIT traces
        final List<LogRecord> monitLogs = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs.size());
        assertMonitProviderFailureLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_DEMANDERCONGES, monitLogs.get(0)), monitLogs.get(1));

        // Check the reply
        final Exception error = responseMsg.getError();
        assertNotNull("No error returns", error);
        assertTrue("Unexpected fault", error.getCause() instanceof NoUserIdValueException);
        assertNull("XML payload in response", responseMsg.getOut());

        assertEquals(currentProcInstNb, this.getProcessInstanceNumber(BPMN_PROCESS_DEFINITION_KEY));
    }

    /**
     * <p>
     * Check the message processing where:
     * <ol>
     * <li>a first valid request is sent to create a new process instance,</li>
     * <li>a 2nd request is sent to complete the waiting user task where the user identifier is missing.</li>
     * </ol>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>on the first request:
     * <ul>
     * <li>the process instance is correctly created in the Activiti engine,</li>
     * <li>the 1st user task is correctly assigned,</li>
     * <li>the service reply is as expected,</li>
     * </ul>
     * </li>
     * <li>on the 2nd request:
     * <ul>
     * <li>a fault is returned</li>
     * <li>the process instance is correctly created in the Activiti engine,</li>
     * <li>the 1st user task is correctly assigned,</li>
     * <li>the service reply is as expected,</li>
     * </ul>
     * </li>
     * </ul>
     * </p>
     */
    @Test
    public void userTaskRequest_NoUserIdValue() throws Exception {

        // Create the 1st valid request
        final Demande request_1 = new Demande();
        request_1.setDemandeur("demandeur");
        request_1.setNbJourDde(10);
        request_1.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(new GregorianCalendar()));
        request_1.setMotifDde("hollidays");

        // Send the 1st valid request
        final RequestToProviderMessage requestM = new RequestToProviderMessage(
                COMPONENT_UNDER_TEST, VALID_SU, OPERATION_DEMANDERCONGES,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(request_1));

        // Assert the response of the 1st valid request
        final ResponseMessage responseMsg_1 = COMPONENT.sendAndGetResponse(requestM);

        // Check the reply
        final Source fault_1 = responseMsg_1.getFault();
        assertNull("Unexpected fault", (fault_1 == null ? null : SourceHelper.toString(fault_1)));
        assertNotNull("No XML payload in response", responseMsg_1.getPayload());
        final Object responseObj_1 = UNMARSHALLER.unmarshal(responseMsg_1.getPayload());
        assertTrue(responseObj_1 instanceof Numero);
        final Numero response_1 = (Numero) responseObj_1;
        assertNotNull(response_1.getNumeroDde());

        COMPONENT.sendDoneStatus(responseMsg_1);

        // Check MONIT traces
        final List<LogRecord> monitLogs_1 = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(4, monitLogs_1.size());
        final FlowLogData processStartedBeginFlowLogData = assertMonitConsumerExtBeginLog(monitLogs_1.get(1));
        assertMonitProviderBeginLog(processStartedBeginFlowLogData, null, null, null, null, monitLogs_1.get(2));
        assertMonitProviderEndLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_DEMANDERCONGES, monitLogs_1.get(0)), monitLogs_1.get(3));

        // Assert that the process instance is created
        assertProcessInstancePending(response_1.getNumeroDde(), BPMN_PROCESS_DEFINITION_KEY);
        assertCurrentUserTask(response_1.getNumeroDde(), BPMN_PROCESS_1ST_USER_TASK_KEY, BPMN_USER_VALIDEUR);

        // Create the 2nd valid request
        final Validation request_2 = new Validation();
        request_2.setNumeroDde(response_1.getNumeroDde());
        request_2.setApprobation(Boolean.TRUE.toString());

        // Send the 2nd valid request
        IN_MEMORY_LOG_HANDLER.clear();

        final RequestToProviderMessage requestM2 = new RequestToProviderMessage(
                COMPONENT_UNDER_TEST, VALID_SU, OPERATION_VALIDERDEMANDE,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(request_2));

        // Assert the response of the 2nd valid request
        final StatusMessage responseMsg_2 = COMPONENT.sendAndGetStatus(requestM2);

        // Check MONIT traces
        final List<LogRecord> monitLogs_2 = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs_2.size());
        assertMonitProviderFailureLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_VALIDERDEMANDE, monitLogs_2.get(0)), monitLogs_2.get(1));

        // Check the reply
        final Exception error = responseMsg_2.getError();
        assertNotNull("No error returns", error);
        assertTrue("Unexpected fault", error.getCause() instanceof NoUserIdValueException);
        assertNull("XML payload in response", responseMsg_2.getOut());

        // Assert that the process instance and current user task
        assertProcessInstancePending(response_1.getNumeroDde(), BPMN_PROCESS_DEFINITION_KEY);
        assertCurrentUserTask(response_1.getNumeroDde(), BPMN_PROCESS_1ST_USER_TASK_KEY, BPMN_USER_VALIDEUR);
    }

    /**
     * <p>
     * Check the message processing where:
     * <ol>
     * <li>a first valid request is sent to create a new process instance,</li>
     * <li>a 2nd request is sent to complete the waiting user task where the user identifier is empty.</li>
     * </ol>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>on the first request:
     * <ul>
     * <li>the process instance is correctly created in the Activiti engine,</li>
     * <li>the 1st user task is correctly assigned,</li>
     * <li>the service reply is as expected,</li>
     * </ul>
     * </li>
     * <li>on the 2nd request:
     * <ul>
     * <li>a fault is returned</li>
     * <li>the process instance is correctly created in the Activiti engine,</li>
     * <li>the 1st user task is correctly assigned,</li>
     * <li>the service reply is as expected,</li>
     * </ul>
     * </li>
     * </ul>
     * </p>
     */
    @Test
    public void userTaskRequest_EmptyUserIdValue() throws Exception {

        // Create the 1st valid request
        final Demande request_1 = new Demande();
        request_1.setDemandeur("demandeur");
        request_1.setNbJourDde(10);
        request_1.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(new GregorianCalendar()));
        request_1.setMotifDde("hollidays");

        // Send the 1st valid request
        final RequestToProviderMessage requestM = new RequestToProviderMessage(
                COMPONENT_UNDER_TEST, VALID_SU, OPERATION_DEMANDERCONGES,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(request_1));

        // Assert the response of the 1st valid request
        final ResponseMessage responseMsg_1 = COMPONENT.sendAndGetResponse(requestM);

        // Check the reply
        final Source fault_1 = responseMsg_1.getFault();
        assertNull("Unexpected fault", (fault_1 == null ? null : SourceHelper.toString(fault_1)));
        assertNotNull("No XML payload in response", responseMsg_1.getPayload());
        final Object responseObj_1 = UNMARSHALLER.unmarshal(responseMsg_1.getPayload());
        assertTrue(responseObj_1 instanceof Numero);
        final Numero response_1 = (Numero) responseObj_1;
        assertNotNull(response_1.getNumeroDde());

        COMPONENT.sendDoneStatus(responseMsg_1);

        // Check MONIT traces
        final List<LogRecord> monitLogs_1 = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(4, monitLogs_1.size());
        final FlowLogData processStartedBeginFlowLogData = assertMonitConsumerExtBeginLog(monitLogs_1.get(1));
        assertMonitProviderBeginLog(processStartedBeginFlowLogData, null, null, null, null, monitLogs_1.get(2));
        assertMonitProviderEndLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_DEMANDERCONGES, monitLogs_1.get(0)), monitLogs_1.get(3));

        // Assert that the process instance is created
        assertProcessInstancePending(response_1.getNumeroDde(), BPMN_PROCESS_DEFINITION_KEY);
        assertCurrentUserTask(response_1.getNumeroDde(), BPMN_PROCESS_1ST_USER_TASK_KEY, BPMN_USER_VALIDEUR);

        // Create the 2nd valid request
        final Validation request_2 = new Validation();
        request_2.setValideur("");
        request_2.setNumeroDde(response_1.getNumeroDde());
        request_2.setApprobation(Boolean.TRUE.toString());

        // Send the 2nd valid request
        IN_MEMORY_LOG_HANDLER.clear();
        final RequestToProviderMessage requestM2 = new RequestToProviderMessage(
                COMPONENT_UNDER_TEST, VALID_SU, OPERATION_VALIDERDEMANDE,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(request_2));

        // Assert the response of the 2nd valid request
        final StatusMessage responseMsg_2 = COMPONENT.sendAndGetStatus(requestM2);

        // Check MONIT traces
        final List<LogRecord> monitLogs_2 = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs_2.size());
        assertMonitProviderFailureLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_VALIDERDEMANDE, monitLogs_2.get(0)), monitLogs_2.get(1));

        // Check the reply
        final Exception error = responseMsg_2.getError();
        assertNotNull("No error returns", error);
        assertTrue("Unexpected fault", error.getCause() instanceof NoUserIdValueException);
        assertNull("XML payload in response", responseMsg_2.getOut());

        // Assert that the process instance and current user task
        assertProcessInstancePending(response_1.getNumeroDde(), BPMN_PROCESS_DEFINITION_KEY);
        assertCurrentUserTask(response_1.getNumeroDde(), BPMN_PROCESS_1ST_USER_TASK_KEY, BPMN_USER_VALIDEUR);
    }

    /**
     * <p>
     * Check the message processing where:
     * <ol>
     * <li>a first valid request is sent to create a new process instance,</li>
     * <li>a 2nd request is sent to complete the waiting user task where the process instance identifier is missing.</li>
     * </ol>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>on the first request:
     * <ul>
     * <li>the process instance is correctly created in the Activiti engine,</li>
     * <li>the 1st user task is correctly assigned,</li>
     * <li>the service reply is as expected,</li>
     * </ul>
     * </li>
     * <li>on the 2nd request:
     * <ul>
     * <li>a fault is returned</li>
     * <li>the process instance is correctly created in the Activiti engine,</li>
     * <li>the 1st user task is correctly assigned,</li>
     * <li>the service reply is as expected,</li>
     * </ul>
     * </li>
     * </ul>
     * </p>
     */
    @Test
    public void userTaskRequest_NoProcessInstanceIdValue() throws Exception {

        // Create the 1st valid request
        final Demande request_1 = new Demande();
        request_1.setDemandeur("demandeur");
        request_1.setNbJourDde(10);
        request_1.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(new GregorianCalendar()));
        request_1.setMotifDde("hollidays");

        // Send the 1st valid request
        final RequestToProviderMessage requestM = new RequestToProviderMessage(
                COMPONENT_UNDER_TEST, VALID_SU, OPERATION_DEMANDERCONGES,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(request_1));

        // Assert the response of the 1st valid request
        final ResponseMessage responseMsg_1 = COMPONENT.sendAndGetResponse(requestM);

        // Check the reply
        final Source fault_1 = responseMsg_1.getFault();
        assertNull("Unexpected fault", (fault_1 == null ? null : SourceHelper.toString(fault_1)));
        assertNotNull("No XML payload in response", responseMsg_1.getPayload());
        final Object responseObj_1 = UNMARSHALLER.unmarshal(responseMsg_1.getPayload());
        assertTrue(responseObj_1 instanceof Numero);
        final Numero response_1 = (Numero) responseObj_1;
        assertNotNull(response_1.getNumeroDde());

        COMPONENT.sendDoneStatus(responseMsg_1);

        // Check MONIT traces
        final List<LogRecord> monitLogs_1 = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(4, monitLogs_1.size());
        final FlowLogData processStartedBeginFlowLogData = assertMonitConsumerExtBeginLog(monitLogs_1.get(1));
        assertMonitProviderBeginLog(processStartedBeginFlowLogData, null, null, null, null, monitLogs_1.get(2));
        assertMonitProviderEndLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_DEMANDERCONGES, monitLogs_1.get(0)), monitLogs_1.get(3));

        // Assert that the process instance is created
        assertProcessInstancePending(response_1.getNumeroDde(), BPMN_PROCESS_DEFINITION_KEY);
        assertCurrentUserTask(response_1.getNumeroDde(), BPMN_PROCESS_1ST_USER_TASK_KEY, BPMN_USER_VALIDEUR);

        // Create the 2nd valid request
        final Validation request_2 = new Validation();
        request_2.setValideur("valideur");
        request_2.setApprobation(Boolean.TRUE.toString());

        // Send the 2nd valid request
        IN_MEMORY_LOG_HANDLER.clear();
        final RequestToProviderMessage requestM2 = new RequestToProviderMessage(
                COMPONENT_UNDER_TEST, VALID_SU, OPERATION_VALIDERDEMANDE,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(request_2));

        // Assert the response of the 2nd valid request
        final StatusMessage responseMsg_2 = COMPONENT.sendAndGetStatus(requestM2);

        // Check MONIT traces
        final List<LogRecord> monitLogs_2 = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs_2.size());
        assertMonitProviderFailureLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_VALIDERDEMANDE, monitLogs_2.get(0)), monitLogs_2.get(1));

        // Check the reply
        final Exception error = responseMsg_2.getError();
        assertNotNull("No error returns", error);
        assertTrue("Unexpected fault", error.getCause() instanceof NoProcessInstanceIdValueException);
        assertNull("XML payload in response", responseMsg_2.getOut());

        // Assert that the process instance and current user task
        assertProcessInstancePending(response_1.getNumeroDde(), BPMN_PROCESS_DEFINITION_KEY);
        assertCurrentUserTask(response_1.getNumeroDde(), BPMN_PROCESS_1ST_USER_TASK_KEY, BPMN_USER_VALIDEUR);
    }

    /**
     * <p>
     * Check the message processing where:
     * <ol>
     * <li>a first valid request is sent to create a new process instance,</li>
     * <li>a 2nd request is sent to complete the waiting user task where the process instance identifier is empty.</li>
     * </ol>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>on the first request:
     * <ul>
     * <li>the process instance is correctly created in the Activiti engine,</li>
     * <li>the 1st user task is correctly assigned,</li>
     * <li>the service reply is as expected,</li>
     * </ul>
     * </li>
     * <li>on the 2nd request:
     * <ul>
     * <li>a fault is returned</li>
     * <li>the process instance is correctly created in the Activiti engine,</li>
     * <li>the 1st user task is correctly assigned,</li>
     * <li>the service reply is as expected,</li>
     * </ul>
     * </li>
     * </ul>
     * </p>
     */
    @Test
    public void userTaskRequest_EmptyProcessInstanceIdValue() throws Exception {

        // Create the 1st valid request
        final Demande request_1 = new Demande();
        request_1.setDemandeur("demandeur");
        request_1.setNbJourDde(10);
        request_1.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(new GregorianCalendar()));
        request_1.setMotifDde("hollidays");

        // Send the 1st valid request
        final RequestToProviderMessage requestM = new RequestToProviderMessage(
                COMPONENT_UNDER_TEST, VALID_SU, OPERATION_DEMANDERCONGES,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(request_1));

        // Assert the response of the 1st valid request
        final ResponseMessage responseMsg_1 = COMPONENT.sendAndGetResponse(requestM);

        // Check the reply
        final Source fault_1 = responseMsg_1.getFault();
        assertNull("Unexpected fault", (fault_1 == null ? null : SourceHelper.toString(fault_1)));
        assertNotNull("No XML payload in response", responseMsg_1.getPayload());
        final Object responseObj_1 = UNMARSHALLER.unmarshal(responseMsg_1.getPayload());
        assertTrue(responseObj_1 instanceof Numero);
        final Numero response_1 = (Numero) responseObj_1;
        assertNotNull(response_1.getNumeroDde());

        COMPONENT.sendDoneStatus(responseMsg_1);

        // Check MONIT traces
        final List<LogRecord> monitLogs_1 = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(4, monitLogs_1.size());
        final FlowLogData processStartedBeginFlowLogData = assertMonitConsumerExtBeginLog(monitLogs_1.get(1));
        assertMonitProviderBeginLog(processStartedBeginFlowLogData, null, null, null, null, monitLogs_1.get(2));
        assertMonitProviderEndLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_DEMANDERCONGES, monitLogs_1.get(0)), monitLogs_1.get(3));

        // Assert that the process instance is created
        assertProcessInstancePending(response_1.getNumeroDde(), BPMN_PROCESS_DEFINITION_KEY);
        assertCurrentUserTask(response_1.getNumeroDde(), BPMN_PROCESS_1ST_USER_TASK_KEY, BPMN_USER_VALIDEUR);

        // Create the 2nd valid request
        final Validation request_2 = new Validation();
        request_2.setValideur("demandeur");
        request_2.setNumeroDde("");
        request_2.setApprobation(Boolean.TRUE.toString());

        // Send the 2nd valid request
        IN_MEMORY_LOG_HANDLER.clear();
        final RequestToProviderMessage requestM2 = new RequestToProviderMessage(
                COMPONENT_UNDER_TEST, VALID_SU, OPERATION_VALIDERDEMANDE,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(request_2));

        // Assert the response of the 2nd valid request
        final StatusMessage responseMsg_2 = COMPONENT.sendAndGetStatus(requestM2);

        // Check MONIT traces
        final List<LogRecord> monitLogs_2 = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs_2.size());
        assertMonitProviderFailureLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_VALIDERDEMANDE, monitLogs_2.get(0)), monitLogs_2.get(1));

        // Check the reply
        final Exception error = responseMsg_2.getError();
        assertNotNull("No error returns", error);
        assertTrue("Unexpected fault", error.getCause() instanceof NoProcessInstanceIdValueException);
        assertNull("XML payload in response", responseMsg_2.getOut());

        // Assert that the process instance and current user task
        assertProcessInstancePending(response_1.getNumeroDde(), BPMN_PROCESS_DEFINITION_KEY);
        assertCurrentUserTask(response_1.getNumeroDde(), BPMN_PROCESS_1ST_USER_TASK_KEY, BPMN_USER_VALIDEUR);
    }

    /**
     * <p>
     * Check the message processing where:
     * <ol>
     * <li>a valid request is sent to complete a waiting user task where the process instance identifier does not exist
     * on the BPMN engine side.</li>
     * </ol>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>the right business fault associated to a process instance identifier not found is returned.</li>
     * </ul>
     * </p>
     */
    @Test
    public void userTaskRequest_ProcessInstanceIdNotFound() throws Exception {

        // Create the valid request
        final Validation request = new Validation();
        request.setValideur("demandeur");
        final String unknownProcessInstanceId = "unknown-processInstanceId";
        request.setNumeroDde(unknownProcessInstanceId);
        request.setApprobation(Boolean.TRUE.toString());

        // Send the 2nd valid request
        final RequestToProviderMessage requestM = new RequestToProviderMessage(
                COMPONENT_UNDER_TEST
                , VALID_SU, OPERATION_VALIDERDEMANDE,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(request));

        // Assert the response of the valid request
        final ResponseMessage responseMsg = COMPONENT.sendAndGetResponse(requestM);

        // Check the reply
        assertNull("Out in response", responseMsg.getOut());
        final Source fault = responseMsg.getFault();
        assertNotNull("No fault returns", fault);
        final Object responseObj = UNMARSHALLER.unmarshal(fault);
        assertTrue(responseObj instanceof NumeroDemandeInconnu);
        final NumeroDemandeInconnu response = (NumeroDemandeInconnu) responseObj;
        assertEquals(unknownProcessInstanceId, response.getNumeroDde());

        COMPONENT.sendDoneStatus(responseMsg);

        // Check MONIT traces
        final List<LogRecord> monitLogs = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs.size());
        assertMonitProviderFailureLog(assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE,
                VACATION_ENDPOINT, OPERATION_VALIDERDEMANDE, monitLogs.get(0)), monitLogs.get(1));

    }

    /**
     * <p>
     * Check the message processing where:
     * <ol>
     * <li>a first valid request is sent to create a new process instance,</li>
     * <li>a 2nd valid request is sent to complete the waiting user task,</li>
     * <li>a 3rd valid request is sent to complete again the user task already completed by the 2nd request.</li>
     * </ol>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>on the first request:
     * <ul>
     * <li>the process instance is correctly created in the Activiti engine,</li>
     * <li>the 1st user task is correctly assigned,</li>
     * <li>the service reply is as expected,</li>
     * </ul>
     * </li>
     * <li>on the 2nd request,:
     * <ul>
     * <li>the process instance is correctly finished in the Activiti engine,</li>
     * <li>the 1st user task is completed by the right assignee,</li>
     * <li>the service reply is as expected,</li>
     * <li>service tasks invoked Petals services</li>
     * </ul>
     * </li>
     * <li>on the 3rd request, the right business fault associated to a user task already completed is returned because
     * the user task is already completed.</li>
     * </ul>
     * </p>
     */
    @Test
    public void userTaskRequest_TaskCompletedFault() throws Exception {

        // --------------------------------------------------------
        // ---- Create a new instance of the process definition
        // --------------------------------------------------------
        // Create the 1st valid request for start event 'request
        final Demande request_1 = new Demande();
        final String demandeur = "demandeur";
        request_1.setDemandeur(demandeur);
        final int numberOfDays = 10;
        request_1.setNbJourDde(numberOfDays);
        final GregorianCalendar now = new GregorianCalendar();
        final GregorianCalendar startDate = new GregorianCalendar(now.get(GregorianCalendar.YEAR),
                now.get(GregorianCalendar.MONTH), now.get(GregorianCalendar.DAY_OF_MONTH));
        request_1.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(startDate));
        final String motivation = "hollidays";
        request_1.setMotifDde(motivation);

        // Send the 1st valid request for start event 'request
        final RequestToProviderMessage requestM = new RequestToProviderMessage(
                COMPONENT_UNDER_TEST, VALID_SU, OPERATION_DEMANDERCONGES,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(request_1));

        // Assert the response of the 1st valid request
        final ResponseMessage responseMsg_1 = COMPONENT.sendAndGetResponse(requestM);

        // Check the reply
        final Source fault_1 = responseMsg_1.getFault();
        assertNull("Unexpected fault", (fault_1 == null ? null : SourceHelper.toString(fault_1)));
        assertNotNull("No XML payload in response", responseMsg_1.getPayload());
        final Object responseObj_1 = UNMARSHALLER.unmarshal(responseMsg_1.getPayload());
        assertTrue(responseObj_1 instanceof Numero);
        final Numero response_1 = (Numero) responseObj_1;
        assertNotNull(response_1.getNumeroDde());

        COMPONENT.sendDoneStatus(responseMsg_1);

        // Check MONIT traces
        final List<LogRecord> monitLogs_1 = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(4, monitLogs_1.size());
        final FlowLogData processStartedBeginFlowLogData = assertMonitConsumerExtBeginLog(monitLogs_1.get(1));
        final FlowLogData userTaskHandleRequestBeginFlowLogData = assertMonitProviderBeginLog(
                processStartedBeginFlowLogData, null, null, null, null, monitLogs_1.get(2));
        assertMonitProviderEndLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_DEMANDERCONGES, monitLogs_1.get(0)), monitLogs_1.get(3));

        // Check Activiti engine against the process instance creation
        assertProcessInstancePending(response_1.getNumeroDde(), BPMN_PROCESS_DEFINITION_KEY);
        assertCurrentUserTask(response_1.getNumeroDde(), BPMN_PROCESS_1ST_USER_TASK_KEY, BPMN_USER_VALIDEUR);

        // ------------------------------------------------------------------
        // ---- Complete the first user task (validate the vacation request)
        // ------------------------------------------------------------------
        // Create the 2nd valid request for the user task 'handleRequest'
        final Validation request_2 = new Validation();
        final String valideur = "valideur";
        request_2.setValideur(valideur);
        request_2.setNumeroDde(response_1.getNumeroDde());
        request_2.setApprobation(Boolean.FALSE.toString());
        request_2.setMotifRefus("To not finished the process and be able to try to complete again the user task");

        // Send the 2nd valid request for the user task 'handleRequest
        IN_MEMORY_LOG_HANDLER.clear();
        final RequestToProviderMessage requestM2 = new RequestToProviderMessage(
                COMPONENT_UNDER_TEST, VALID_SU, OPERATION_VALIDERDEMANDE,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(request_2));

        // Assert the response of the 2nd valid request
        final ResponseMessage responseMsg_2 = COMPONENT.sendAndGetResponse(requestM2);

        // Check the reply
        final Source fault_2 = responseMsg_2.getFault();
        assertNull("Unexpected fault", (fault_2 == null ? null : SourceHelper.toString(fault_2)));
        assertNotNull("No XML payload in response", responseMsg_2.getPayload());
        final Object responseObj_2 = UNMARSHALLER.unmarshal(responseMsg_2.getPayload());
        assertTrue(responseObj_2 instanceof AckResponse);
        final AckResponse response_2 = (AckResponse) responseObj_2;
        assertNotNull(response_2);

        COMPONENT.sendDoneStatus(responseMsg_2);

        // Check MONIT traces
        final List<LogRecord> monitLogs_2 = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(4, monitLogs_2.size());
        assertMonitProviderEndLog(userTaskHandleRequestBeginFlowLogData, monitLogs_2.get(1));
        final FlowLogData secondUserTaskHandleRequestBeginFlowLogData = assertMonitProviderBeginLog(
                processStartedBeginFlowLogData, null, null, null, null, monitLogs_1.get(2));
        assertMonitProviderEndLog(
                assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                        OPERATION_VALIDERDEMANDE, monitLogs_2.get(0)), monitLogs_2.get(3));

        // Check Activiti engine against the current user task
        assertProcessInstanceFinished(response_1.getNumeroDde());
        assertUserTaskEnded(response_1.getNumeroDde(), BPMN_PROCESS_2ND_USER_TASK_KEY, BPMN_USER_DEMANDEUR);

        // --------------------------------------------------------
        // ---- Try to complete AGAIN the first user task
        // --------------------------------------------------------
        // Create the 3rd request for the user task 'handleRequest'
        final Validation request_3 = new Validation();
        request_3.setValideur(valideur);
        request_3.setNumeroDde(response_1.getNumeroDde());
        request_3.setApprobation(Boolean.TRUE.toString());
        request_2.setMotifRefus("On this 2nd call a fault should occur completing the user task");

        // Send the 3rd valid request for the user task 'handleRequest'
        IN_MEMORY_LOG_HANDLER.clear();
        final RequestToProviderMessage requestM3 = new RequestToProviderMessage(
                COMPONENT_UNDER_TEST, VALID_SU, OPERATION_VALIDERDEMANDE,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(request_3));

        // Assert the response of the 3rd valid request
        final ResponseMessage responseMsg_3 = COMPONENT.sendAndGetResponse(requestM3);

        // Check the reply
        assertNull("Out in response", responseMsg_3.getOut());
        final Source fault_3 = responseMsg_3.getFault();
        assertNotNull("No fault returns", fault_3);
        final Object responseObj_3 = UNMARSHALLER.unmarshal(fault_3);
        assertTrue(responseObj_3 instanceof DemandeDejaValidee);
        final DemandeDejaValidee response_3 = (DemandeDejaValidee) responseObj_3;
        assertEquals(response_1.getNumeroDde(), response_3.getNumeroDde());

        COMPONENT.sendDoneStatus(responseMsg_3);

        // Check MONIT traces
        final List<LogRecord> monitLogs_3 = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs_3.size());
        assertMonitProviderFailureLog(assertMonitProviderBeginLog(VACATION_INTERFACE, VACATION_SERVICE,
                VACATION_ENDPOINT, OPERATION_VALIDERDEMANDE, monitLogs_3.get(0)), monitLogs_3.get(1));

        // Check Activiti engine against the current user task
        assertProcessInstanceFinished(response_1.getNumeroDde());
        assertUserTaskEnded(response_1.getNumeroDde(), BPMN_PROCESS_2ND_USER_TASK_KEY, BPMN_USER_DEMANDEUR);
    }

    /**
     * <p>
     * Unit test about:
     * <ul>
     * <li>PETALSSEACTIVITI-4: a service task follow the start event,</li>
     * <li>PETALSSEACTIVITI-5: process definition without user task.</li>
     * </ul>
     * </p>
     */
    @Test
    public void jira_PETALSSEACTIVITI_4() throws Exception {

        // --------------------------------------------------------
        // ---- Create a new instance of the process definition
        // --------------------------------------------------------
        final JiraPETALSSEACTIVITI4 request_1 = new JiraPETALSSEACTIVITI4();
        request_1.setDemandeur(BPMN_USER_DEMANDEUR);
        final String numberOfDays = "10";
        request_1.setNbJourDde(numberOfDays);

        final ServiceProviderImplementation service = new ServiceProviderImplementation() {

            private MessageExchange archiveMessageExchange_1;

            @Override
            public Message provides(RequestMessage archiveRequestMsg_1) throws Exception {
                // Assert the 1st request sent by Activiti on orchestrated service
                assertNotNull("No service request received under the given delay", archiveRequestMsg_1);
                archiveMessageExchange_1 = archiveRequestMsg_1.getMessageExchange();
                assertNotNull(archiveMessageExchange_1);
                assertEquals(ARCHIVE_INTERFACE, archiveMessageExchange_1.getInterfaceName());
                assertEquals(ARCHIVE_SERVICE, archiveMessageExchange_1.getService());
                assertNotNull(archiveMessageExchange_1.getEndpoint());
                assertEquals(ARCHIVE_ENDPOINT, archiveMessageExchange_1.getEndpoint().getEndpointName());
                assertEquals(ARCHIVER_OPERATION, archiveMessageExchange_1.getOperation());
                assertEquals(archiveMessageExchange_1.getStatus(), ExchangeStatus.ACTIVE);
                final Object archiveRequestObj_1 = UNMARSHALLER
                        .unmarshal(archiveRequestMsg_1.getPayload());
                assertTrue(archiveRequestObj_1 instanceof Archiver);
                final Archiver archiveRequest_1 = (Archiver) archiveRequestObj_1;
                assertEquals(numberOfDays, archiveRequest_1.getItem());

                // Returns the reply of the service provider to the Activiti service task
                final ArchiverResponse archiverResponse_1 = new ArchiverResponse();
                archiverResponse_1.setItem("value of item");
                archiverResponse_1.setItem2("value of item2");
                return new ResponseToConsumerMessage(archiveRequestMsg_1, toByteArray(archiverResponse_1));
            }

            @Override
            public void handleStatus(StatusMessage statusDoneMsg_1) throws Exception {
                // Assert the status DONE on the message exchange
                assertNotNull(statusDoneMsg_1);
                // It's the same message exchange instance
                assertSame(statusDoneMsg_1.getMessageExchange(), archiveMessageExchange_1);
                assertEquals(statusDoneMsg_1.getMessageExchange().getStatus(), ExchangeStatus.DONE);
            }
        };

        // Send the 1st valid request for start event 'request'
        final RequestToProviderMessage request = new RequestToProviderMessage(
                COMPONENT_UNDER_TEST, VALID_SU, OPERATION_JIRA,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(request_1));

        final ResponseMessage responseMsg_1 = COMPONENT.sendAndGetResponse(request, service);

        // Check the reply
        final Source fault_1 = responseMsg_1.getFault();
        assertNull("Unexpected fault", (fault_1 == null ? null : SourceHelper.toString(fault_1)));
        assertNotNull("No XML payload in response", responseMsg_1.getPayload());
        final Object responseObj_1 = UNMARSHALLER.unmarshal(responseMsg_1.getPayload());
        assertTrue(responseObj_1 instanceof Numero);
        final Numero response_1 = (Numero) responseObj_1;
        assertNotNull(response_1.getNumeroDde());
        {
            assertEquals(5, response_1.getXslParameter().size());
            boolean userFound = false;
            boolean employeeFound = false;
            boolean numberOfDaysFound = false;
            for (final XslParameter xslParameter : response_1.getXslParameter()) {
                if ("user-id".equals(xslParameter.getName().toString())) {
                    userFound = true;
                    assertEquals(BPMN_USER_DEMANDEUR, xslParameter.getValue());
                } else if ("employeeName".equals(xslParameter.getName().toString())) {
                    employeeFound = true;
                    assertEquals(BPMN_USER_DEMANDEUR, xslParameter.getValue());
                } else if ("numberOfDays".equals(xslParameter.getName().toString())) {
                    numberOfDaysFound = true;
                    assertEquals(numberOfDays, xslParameter.getValue());
                }
            }
            assertTrue(userFound);
            assertTrue(employeeFound);
            assertTrue(numberOfDaysFound);
        }

        COMPONENT.sendDoneStatus(responseMsg_1, service);

        // Wait end of process instance
        this.waitEndOfProcessInstance(response_1.getNumeroDde());

        
        // Check MONIT traces
        final List<LogRecord> monitLogs_1 = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(6, monitLogs_1.size());
        final FlowLogData initialInteractionRequestFlowLogData = assertMonitProviderBeginLog(VACATION_INTERFACE,
                VACATION_SERVICE, VACATION_ENDPOINT, OPERATION_JIRA, monitLogs_1.get(0));
        final FlowLogData processStartedBeginFlowLogData = assertMonitConsumerExtBeginLog(monitLogs_1.get(1));
        assertEquals(initialInteractionRequestFlowLogData.get(FlowLogData.FLOW_INSTANCE_ID_PROPERTY_NAME),
                processStartedBeginFlowLogData.get(ActivitiActivityFlowStepData.CORRELATED_FLOW_INSTANCE_ID_KEY));
        assertEquals(initialInteractionRequestFlowLogData.get(FlowLogData.FLOW_STEP_ID_PROPERTY_NAME),
                processStartedBeginFlowLogData.get(ActivitiActivityFlowStepData.CORRELATED_FLOW_STEP_ID_KEY));
        assertEquals("jira_PETALSSEACTIVITI-4",
                processStartedBeginFlowLogData.get(ProcessInstanceFlowStepBeginLogData.PROCESS_DEFINITION_KEY));
        assertEquals(response_1.getNumeroDde(),
                processStartedBeginFlowLogData.get(ProcessInstanceFlowStepBeginLogData.PROCESS_INSTANCE_ID_KEY));
        assertMonitProviderEndLog(initialInteractionRequestFlowLogData, monitLogs_1.get(2));
        // TODO: Should we have MONIT traces about the service call on the consumer side ?
        // Check the MONIT traces of the invoked services: the service is responsible to generate them, not the SE
        // Activiti.
        final FlowLogData serviceTaskFlowLogData = assertMonitProviderBeginLog(processStartedBeginFlowLogData,
                ARCHIVE_INTERFACE, ARCHIVE_SERVICE, ARCHIVE_ENDPOINT, ARCHIVER_OPERATION, monitLogs_1.get(3));
        assertMonitProviderEndLog(serviceTaskFlowLogData, monitLogs_1.get(4));
        // Check the last MONIT trace of the BPMN process
        assertMonitConsumerExtEndLog(processStartedBeginFlowLogData, monitLogs_1.get(5));
    }

    /**
     * Retrieve a process instance.
     * 
     * @param processInstanceId
     *            The process instance identifier of the process instance to retrieve
     * @param expectedStartDate
     *            The expected start date of the retrieved process instance.
     * @param expectedMotivation
     *            The expected value of the field 'motivation' of the retrieved process instance.
     * @param expectedNumberOfDays
     *            The expected value of the field 'numberOfDays' of the retrieved process instance.
     * @param state
     *            {@link ProcessInstanceState} of the process instances to retrieve.
     * @param processStartedBeginFlowLogData
     *            The first MONIT trace of the process instance execution.
     * @param isRetrievedProcessInstanceExpected
     *            Expected result: <code>true</code> if a process instance must be retrieved; <code>false</code> if no
     *            process instance must be retrieved
     * @throws JAXBException
     * @throws IOException
     */
    private final void tryToRetrieveProcessInstance(final String processInstanceId,
            final GregorianCalendar expectedStartDate, final String expectedMotivation, final int expectedNumberOfDays,
            final ProcessInstanceState state, final FlowLogData processStartedBeginFlowLogData,
            final boolean isRetrievedProcessInstanceExpected) throws Exception {

        // Try to retrieve the process instance using the integration service
        final GetProcessInstances getProcessInstancesReq = new GetProcessInstances();
        getProcessInstancesReq.setState(state);
        getProcessInstancesReq.setProcessDefinitionIdentifier(BPMN_PROCESS_DEFINITION_KEY);
        getProcessInstancesReq.setProcessInstanceIdentifier(processInstanceId);

        IN_MEMORY_LOG_HANDLER.clear();
        final RequestToProviderMessage request = new RequestToProviderMessage(
                COMPONENT_UNDER_TEST, NATIVE_PROCESSINSTANCES_SVC_CFG,
                ITG_OP_GETPROCESSINSTANCES, AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                toByteArray(getProcessInstancesReq));

        {
            final ResponseMessage getProcessInstancesRespMsg = COMPONENT.sendAndGetResponse(request);
            assertNotNull("No XML payload in response", getProcessInstancesRespMsg.getPayload());
            final Object getProcessInstancesRespObj = UNMARSHALLER
                    .unmarshal(getProcessInstancesRespMsg.getPayload());
            assertTrue(getProcessInstancesRespObj instanceof GetProcessInstancesResponse);
            final GetProcessInstancesResponse getProcessInstancesResp = (GetProcessInstancesResponse) getProcessInstancesRespObj;
            assertNotNull(getProcessInstancesResp.getProcessInstances());
            assertNotNull(getProcessInstancesResp.getProcessInstances().getProcessInstance());
            if (isRetrievedProcessInstanceExpected) {
                assertEquals(1, getProcessInstancesResp.getProcessInstances().getProcessInstance().size());
                final ProcessInstance processInstance = getProcessInstancesResp.getProcessInstances()
                        .getProcessInstance().get(0);
                assertEquals(BPMN_PROCESS_DEFINITION_KEY, processInstance.getProcessDefinitionIdentifier());
                assertEquals(processInstanceId, processInstance.getProcessInstanceIdentifier());
                assertNotNull(processInstance.getVariables());
                final List<Variable> variables = processInstance.getVariables().getVariable();
                assertNotNull(variables);
                assertTrue(variables.size() > 5);
                for (final Variable variable : variables) {
                    if ("startDate".equals(variable.getName())) {
                        assertEquals(
                                0,
                                expectedStartDate.compareTo(DatatypeFactory.newInstance()
                                        .newXMLGregorianCalendar(variable.getValue()).toGregorianCalendar()));
                    } else if ("vacationMotivation".equals(variable.getName())) {
                        assertEquals(expectedMotivation, variable.getValue());
                    } else if ("numberOfDays".equals(variable.getName())) {
                        assertEquals(String.valueOf(expectedNumberOfDays), variable.getValue());
                    } else if ("employeeName".equals(variable.getName())) {
                        assertEquals(BPMN_USER_DEMANDEUR, variable.getValue());
                    } else if ("petals.flow.instance.id".equals(variable.getName())) {
                        assertEquals(
                                processStartedBeginFlowLogData
                                        .get(FlowLogData.FLOW_INSTANCE_ID_PROPERTY_NAME),
                                variable.getValue());
                    }
                }
            } else {
                assertEquals(0, getProcessInstancesResp.getProcessInstances().getProcessInstance().size());
            }

            COMPONENT.sendDoneStatus(getProcessInstancesRespMsg);
        }

        // Check MONIT traces about the service integration invocation
        final List<LogRecord> monitLogs_getProcessInstances = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs_getProcessInstances.size());
        final FlowLogData providerBegin_getProcessInstances = assertMonitProviderBeginLog(
                ITG_PROCESSINSTANCES_PORT_TYPE, ITG_PROCESSINSTANCES_SERVICE,
                COMPONENT_UNDER_TEST.getNativeEndpointName(ITG_PROCESSINSTANCES_SERVICE), ITG_OP_GETPROCESSINSTANCES,
                monitLogs_getProcessInstances.get(0));
        assertMonitProviderEndLog(providerBegin_getProcessInstances, monitLogs_getProcessInstances.get(1));
        assertMonitFlowInstanceIdNotEquals(processStartedBeginFlowLogData, providerBegin_getProcessInstances);
    }

    /**
     * Retrieve a user task.
     * 
     * @param user
     *            The user to which the task is assigned
     * @param processInstanceId
     *            The process instance of the task to retrieve
     * @param isActive
     *            <code>true</code> if the process instance is active, <code>false</code> if it is suspended,
     * @param processStartedBeginFlowLogData
     *            The first MONIT trace of the process instance execution.
     * @param isRetrievedTaskExpected
     *            Expected result: <code>true</code> if a task must be retrieved; <code>false</code> if no task must be
     *            retrieved
     * @throws JAXBException
     * @throws IOException
     */
    private final void retrieveUserTask(final String user, final String processInstanceId, final boolean isActive,
            final FlowLogData processStartedBeginFlowLogData, final boolean isRetrievedTaskExpected) throws Exception {

        // Verify the task basket using integration service
        final GetTasks getTasksReq = new GetTasks();
        getTasksReq.setActive(isActive);
        getTasksReq.setAssignee(user);
        getTasksReq.setProcessInstanceIdentifier(processInstanceId);

        IN_MEMORY_LOG_HANDLER.clear();
        final RequestToProviderMessage requestM = new RequestToProviderMessage(
                COMPONENT_UNDER_TEST, NATIVE_TASKS_SVC_CFG, ITG_OP_GETTASKS,
                AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(getTasksReq));

        {
            final ResponseMessage getTaskRespMsg = COMPONENT.sendAndGetResponse(requestM);
            assertNotNull("No XML payload in response", getTaskRespMsg.getPayload());
            final Object getTaskRespObj = UNMARSHALLER.unmarshal(getTaskRespMsg.getPayload());
            assertTrue(getTaskRespObj instanceof GetTasksResponse);
            final GetTasksResponse getTaskResp = (GetTasksResponse) getTaskRespObj;
            assertNotNull(getTaskResp.getTasks());
            assertNotNull(getTaskResp.getTasks().getTask());
            if (isRetrievedTaskExpected) {
                assertEquals(1, getTaskResp.getTasks().getTask().size());
                final Task task = getTaskResp.getTasks().getTask().get(0);
                assertEquals(BPMN_PROCESS_DEFINITION_KEY, task.getProcessDefinitionIdentifier());
                assertEquals(processInstanceId, task.getProcessInstanceIdentifier());
                assertEquals(BPMN_PROCESS_1ST_USER_TASK_KEY, task.getTaskIdentifier());
            } else {
                assertEquals(0, getTaskResp.getTasks().getTask().size());
            }

            COMPONENT.sendDoneStatus(getTaskRespMsg);
        }
        // Check MONIT traces about the task basket invocation
        final List<LogRecord> monitLogs_taskBasket = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs_taskBasket.size());
        final FlowLogData providerBegin_taskBasket = assertMonitProviderBeginLog(ITG_TASK_PORT_TYPE, ITG_TASK_SERVICE,
                COMPONENT_UNDER_TEST.getNativeEndpointName(ITG_TASK_SERVICE), ITG_OP_GETTASKS,
                monitLogs_taskBasket.get(0));
        assertMonitProviderEndLog(providerBegin_taskBasket, monitLogs_taskBasket.get(1));
        assertMonitFlowInstanceIdNotEquals(processStartedBeginFlowLogData, providerBegin_taskBasket);
    }

    private final void tryToSuspendProcessInstance(final String processInstanceId,
            final FlowLogData processStartedBeginFlowLogData, final AdjournmentResult expectedResult) throws Exception {

        if (expectedResult == AdjournmentResult.SUSPENDED) {
            // Check at Activiti level that the process instance is suspended
            final List<org.activiti.engine.runtime.ProcessInstance> processInstances = this.activitiClient
                    .getRuntimeService().createProcessInstanceQuery().processInstanceId(processInstanceId).list();
            assertNotNull(processInstances);
            assertEquals(1, processInstances.size());
            assertFalse(processInstances.get(0).isSuspended());
        }

        final SuspendProcessInstances suspendProcessInstancesReq = new SuspendProcessInstances();
        suspendProcessInstancesReq.getProcessInstanceIdentifier().add(processInstanceId);

        IN_MEMORY_LOG_HANDLER.clear();
        final RequestToProviderMessage requestM = new RequestToProviderMessage(
                COMPONENT_UNDER_TEST, NATIVE_PROCESSINSTANCES_SVC_CFG,
                ITG_OP_SUSPENDPROCESSINSTANCES, AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                toByteArray(suspendProcessInstancesReq));

        {
            final ResponseMessage suspendProcessInstancesRespMsg = COMPONENT.sendAndGetResponse(requestM);
            assertNotNull("No XML payload in response", suspendProcessInstancesRespMsg.getPayload());
            final Object suspendProcessInstancesRespObj = UNMARSHALLER
                    .unmarshal(suspendProcessInstancesRespMsg.getPayload());
            assertTrue(suspendProcessInstancesRespObj instanceof SuspendProcessInstancesResponse);
            final SuspendProcessInstancesResponse suspendProcessInstancesResp = (SuspendProcessInstancesResponse) suspendProcessInstancesRespObj;
            assertNotNull(suspendProcessInstancesResp.getProcessInstanceIdentifier());
            assertEquals(1, suspendProcessInstancesResp.getProcessInstanceIdentifier().size());
            assertNotNull(suspendProcessInstancesResp.getProcessInstanceIdentifier().get(0));
            assertEquals(processInstanceId, suspendProcessInstancesResp.getProcessInstanceIdentifier().get(0)
                    .getValue());
            assertEquals(expectedResult, suspendProcessInstancesResp.getProcessInstanceIdentifier().get(0).getResult());

            COMPONENT.sendDoneStatus(suspendProcessInstancesRespMsg);
        }

        // Check MONIT traces about the process instance adjournment
        final List<LogRecord> monitLogs_suspendProcessInstance = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs_suspendProcessInstance.size());
        final FlowLogData providerBegin_suspendProcessInstance = assertMonitProviderBeginLog(
                ITG_PROCESSINSTANCES_PORT_TYPE, ITG_PROCESSINSTANCES_SERVICE,
                COMPONENT_UNDER_TEST.getNativeEndpointName(ITG_PROCESSINSTANCES_SERVICE),
                ITG_OP_SUSPENDPROCESSINSTANCES, monitLogs_suspendProcessInstance.get(0));
        assertMonitProviderEndLog(providerBegin_suspendProcessInstance, monitLogs_suspendProcessInstance.get(1));
        assertMonitFlowInstanceIdNotEquals(processStartedBeginFlowLogData, providerBegin_suspendProcessInstance);

        if (expectedResult == AdjournmentResult.SUSPENDED) {
            // Check at Activiti level that the process instance is suspended
            final List<org.activiti.engine.runtime.ProcessInstance> processInstances = this.activitiClient
                    .getRuntimeService().createProcessInstanceQuery().processInstanceId(processInstanceId).list();
            assertNotNull(processInstances);
            assertEquals(1, processInstances.size());
            assertTrue(processInstances.get(0).isSuspended());
        }
    }

    private final void tryToActivateProcessInstance(final String processInstanceId,
            final FlowLogData processStartedBeginFlowLogData, final ActivationResult expectedResult) throws Exception {

        if (expectedResult == ActivationResult.ACTIVATED) {
            // Check at Activiti level that the process instance is activated
            final List<org.activiti.engine.runtime.ProcessInstance> processInstances = this.activitiClient
                    .getRuntimeService().createProcessInstanceQuery().processInstanceId(processInstanceId).list();
            assertNotNull(processInstances);
            assertEquals(1, processInstances.size());
            assertTrue(processInstances.get(0).isSuspended());
        }

        final ActivateProcessInstances activateProcessInstancesReq = new ActivateProcessInstances();
        activateProcessInstancesReq.getProcessInstanceIdentifier().add(processInstanceId);

        IN_MEMORY_LOG_HANDLER.clear();
        final RequestToProviderMessage requestM = new RequestToProviderMessage(
                COMPONENT_UNDER_TEST, NATIVE_PROCESSINSTANCES_SVC_CFG,
                ITG_OP_ACTIVATEPROCESSINSTANCES, AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                toByteArray(activateProcessInstancesReq));

        {
            final ResponseMessage activateProcessInstancesRespMsg = COMPONENT.sendAndGetResponse(requestM);
            assertNotNull("No XML payload in response", activateProcessInstancesRespMsg.getPayload());
            final Object activateProcessInstancesRespObj = UNMARSHALLER
                    .unmarshal(activateProcessInstancesRespMsg.getPayload());
            assertTrue(activateProcessInstancesRespObj instanceof ActivateProcessInstancesResponse);
            final ActivateProcessInstancesResponse activateProcessInstancesResp = (ActivateProcessInstancesResponse) activateProcessInstancesRespObj;
            assertNotNull(activateProcessInstancesResp.getProcessInstanceIdentifier());
            assertEquals(1, activateProcessInstancesResp.getProcessInstanceIdentifier().size());
            assertNotNull(activateProcessInstancesResp.getProcessInstanceIdentifier().get(0));
            assertEquals(processInstanceId, activateProcessInstancesResp.getProcessInstanceIdentifier().get(0)
                    .getValue());
            assertEquals(expectedResult, activateProcessInstancesResp.getProcessInstanceIdentifier().get(0).getResult());

            COMPONENT.sendDoneStatus(activateProcessInstancesRespMsg);
        }

        // Check MONIT traces about the process instance activation
        final List<LogRecord> monitLogs_activateProcessInstance = IN_MEMORY_LOG_HANDLER.getAllRecords(Level.MONIT);
        assertEquals(2, monitLogs_activateProcessInstance.size());
        final FlowLogData providerBegin_activateProcessInstance = assertMonitProviderBeginLog(
                ITG_PROCESSINSTANCES_PORT_TYPE, ITG_PROCESSINSTANCES_SERVICE,
                COMPONENT_UNDER_TEST.getNativeEndpointName(ITG_PROCESSINSTANCES_SERVICE),
                ITG_OP_ACTIVATEPROCESSINSTANCES, monitLogs_activateProcessInstance.get(0));
        assertMonitProviderEndLog(providerBegin_activateProcessInstance, monitLogs_activateProcessInstance.get(1));
        assertMonitFlowInstanceIdNotEquals(processStartedBeginFlowLogData, providerBegin_activateProcessInstance);

        if (expectedResult == ActivationResult.ACTIVATED) {
            // Check at Activiti level that the process instance is activated
            final List<org.activiti.engine.runtime.ProcessInstance> processInstances = this.activitiClient
                    .getRuntimeService().createProcessInstanceQuery().processInstanceId(processInstanceId).list();
            assertNotNull(processInstances);
            assertEquals(1, processInstances.size());
            assertFalse(processInstances.get(0).isSuspended());
        }
    }
}

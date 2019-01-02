/**
 * Copyright (c) 2014-2019 Linagora
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
package org.ow2.petals.flowable;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URL;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.namespace.QName;

import org.ow2.petals.component.framework.junit.impl.ConsumesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ProvidesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration;
import org.ow2.petals.component.framework.junit.rule.ServiceConfigurationFactory;
import org.ow2.petals.components.flowable.generic._1.ActivateProcessInstances;
import org.ow2.petals.components.flowable.generic._1.ActivateProcessInstancesResponse;
import org.ow2.petals.components.flowable.generic._1.GetProcessInstances;
import org.ow2.petals.components.flowable.generic._1.GetProcessInstancesResponse;
import org.ow2.petals.components.flowable.generic._1.GetTasks;
import org.ow2.petals.components.flowable.generic._1.GetTasksResponse;
import org.ow2.petals.components.flowable.generic._1.GetUser;
import org.ow2.petals.components.flowable.generic._1.GetUserResponse;
import org.ow2.petals.components.flowable.generic._1.InvalidRequest;
import org.ow2.petals.components.flowable.generic._1.SearchGroups;
import org.ow2.petals.components.flowable.generic._1.SearchGroupsResponse;
import org.ow2.petals.components.flowable.generic._1.SearchUsers;
import org.ow2.petals.components.flowable.generic._1.SearchUsersResponse;
import org.ow2.petals.components.flowable.generic._1.SuspendProcessInstances;
import org.ow2.petals.components.flowable.generic._1.SuspendProcessInstancesResponse;
import org.ow2.petals.components.flowable.generic._1.UnknownUser;
import org.ow2.petals.se_flowable.unit_test.vacation.archivageservice.Archiver;
import org.ow2.petals.se_flowable.unit_test.vacation.archivageservice.ArchiverResponse;
import org.ow2.petals.se_flowable.unit_test.vacation.vacationservice.AckResponse;
import org.ow2.petals.se_flowable.unit_test.vacation.vacationservice.Demande;
import org.ow2.petals.se_flowable.unit_test.vacation.vacationservice.DemandeDejaValidee;
import org.ow2.petals.se_flowable.unit_test.vacation.vacationservice.JiraPETALSSEACTIVITI4;
import org.ow2.petals.se_flowable.unit_test.vacation.vacationservice.Numero;
import org.ow2.petals.se_flowable.unit_test.vacation.vacationservice.NumeroDemandeInconnu;
import org.ow2.petals.se_flowable.unit_test.vacation.vacationservice.Validation;

import com.ebmwebsourcing.easycommons.lang.UncheckedException;

/**
 * Abstract class for unit tests about request processing
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public abstract class AbstractVacationProcessTestEnvironment extends AbstractTestEnvironment {

    protected static final String VACATION_SU = "vacation-su";

    private static final String VACATION_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/vacation/vacationService";

    protected static final QName VACATION_INTERFACE = new QName(VACATION_NAMESPACE, "demandeDeConges");

    protected static final QName VACATION_SERVICE = new QName(VACATION_NAMESPACE, "demandeDeCongesService");

    protected static final String VACATION_ENDPOINT = "testEndpointName";

    protected static final QName OPERATION_JIRA = new QName(VACATION_NAMESPACE, "jira_PETALSSEACTIVITI-4");

    protected static final QName OPERATION_DEMANDERCONGES = new QName(VACATION_NAMESPACE, "demanderConges");

    protected static final QName OPERATION_VALIDERDEMANDE = new QName(VACATION_NAMESPACE, "validerDemande");

    public static final String BPMN_PROCESS_DEFINITION_KEY = "vacationRequest";

    protected static final String BPMN_PROCESS_1ST_USER_TASK_KEY = "handleRequest";

    protected static final String BPMN_PROCESS_2ND_USER_TASK_KEY = "adjustVacationRequestTask";

    public static final String BPMN_USER_DEMANDEUR = "demandeur";

    public static final String BPMN_USER_VALIDEUR = "valideur";

    private static final String ARCHIVE_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/vacation/archivageService";

    protected static final QName ARCHIVE_INTERFACE = new QName(ARCHIVE_NAMESPACE, "archiver");

    protected static final QName ARCHIVE_SERVICE = new QName(ARCHIVE_NAMESPACE, "archiverService");

    protected static final String ARCHIVE_ENDPOINT = "archiveEndpointName";

    protected static final QName ARCHIVER_OPERATION = new QName(ARCHIVE_NAMESPACE, "archiver");

    protected static ServiceConfigurationFactory createVacationServiceCfgFactory() {

        return createVacationServiceCfgFactory(1);
    }

    protected static ServiceConfigurationFactory createVacationServiceCfgFactory(final int bpmnVersion) {

        return new ServiceConfigurationFactory() {
            @Override
            public ServiceConfiguration create() {

                final URL wsdlUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(VACATION_SU_HOME + "vacationRequest.wsdl");
                assertNotNull("WSDl not found", wsdlUrl);
                final ProvidesServiceConfiguration serviceConfiguration = new ProvidesServiceConfiguration(
                        VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT, wsdlUrl);

                final URL demanderCongesResponseXslUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(VACATION_SU_HOME + "demanderCongesResponse.xsl");
                assertNotNull("Output XSL 'demanderCongesResponse.xsl' not found", demanderCongesResponseXslUrl);
                serviceConfiguration.addResource(demanderCongesResponseXslUrl);

                final URL validerDemandeResponseXslUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(VACATION_SU_HOME + "validerDemandeResponse.xsl");
                assertNotNull("Output XSL 'validerDemandeResponse.xsl' not found", validerDemandeResponseXslUrl);
                serviceConfiguration.addResource(validerDemandeResponseXslUrl);

                final URL ajusterDemandeResponseXslUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(VACATION_SU_HOME + "ajusterDemandeResponse.xsl");
                assertNotNull("Output XSL 'ajusterDemandeResponse.xsl' not found", ajusterDemandeResponseXslUrl);
                serviceConfiguration.addResource(ajusterDemandeResponseXslUrl);

                final URL numeroDemandeInconnuXslUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(VACATION_SU_HOME + "numeroDemandeInconnu.xsl");
                assertNotNull("Output XSL 'numeroDemandeInconnu.xsl' not found", numeroDemandeInconnuXslUrl);
                serviceConfiguration.addResource(numeroDemandeInconnuXslUrl);

                final URL demandeDejaValideeXslUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(VACATION_SU_HOME + "demandeDejaValidee.xsl");
                assertNotNull("Output XSL 'demandeDejaValidee.xsl' not found", demandeDejaValideeXslUrl);
                serviceConfiguration.addResource(demandeDejaValideeXslUrl);

                final URL bpmnUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(VACATION_SU_HOME + "vacationRequest.bpmn20.xml");
                assertNotNull("BPMN file not found", bpmnUrl);
                serviceConfiguration.addResource(bpmnUrl);

                final URL archivageServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(VACATION_SU_HOME + "archivageService.wsdl");
                assertNotNull("archivageService WSDL not found", archivageServiceWsdlUrl);
                serviceConfiguration.addResource(archivageServiceWsdlUrl);

                serviceConfiguration.setServicesSectionParameter(
                        new QName(FlowableSEConstants.NAMESPACE_SU, "process_file"), "vacationRequest.bpmn20.xml");
                serviceConfiguration.setServicesSectionParameter(new QName(FlowableSEConstants.NAMESPACE_SU, "version"),
                        String.valueOf(bpmnVersion));

                // Consume service 'archiver'
                // TODO: The consume section seems mandatory to retrieve the consume endpoint on async exchange
                // between Flowable and other services
                final ConsumesServiceConfiguration consumeServiceConfiguration = new ConsumesServiceConfiguration(
                        ARCHIVE_INTERFACE, ARCHIVE_SERVICE, ARCHIVE_ENDPOINT);
                serviceConfiguration.addServiceConfigurationDependency(consumeServiceConfiguration);

                return serviceConfiguration;
            }
        };
    }

    protected static Marshaller MARSHALLER;

    protected static Unmarshaller UNMARSHALLER;

    static {
        try {
            final JAXBContext context = JAXBContext.newInstance(Demande.class, Validation.class, Numero.class,
                    AckResponse.class, NumeroDemandeInconnu.class, DemandeDejaValidee.class, Archiver.class,
                    ArchiverResponse.class, GetTasks.class, GetTasksResponse.class, GetProcessInstances.class,
                    GetProcessInstancesResponse.class, JiraPETALSSEACTIVITI4.class, InvalidRequest.class,
                    SuspendProcessInstances.class, SuspendProcessInstancesResponse.class,
                    ActivateProcessInstances.class, ActivateProcessInstancesResponse.class, GetUser.class,
                    GetUserResponse.class, UnknownUser.class, SearchUsers.class, SearchUsersResponse.class,
                    SearchGroups.class, SearchGroupsResponse.class);
            UNMARSHALLER = context.createUnmarshaller();
            MARSHALLER = context.createMarshaller();
            MARSHALLER.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
        } catch (final JAXBException e) {
            throw new UncheckedException(e);
        }
    }

    public AbstractVacationProcessTestEnvironment() {
        super(VACATION_SU_HOME + "idm-engine-configurator.properties");
    }

    /**
     * Convert a JAXB element to bytes
     * 
     * @param jaxbElement
     *            The JAXB element to write as bytes
     */
    protected static byte[] toByteArray(final Object jaxbElement) throws JAXBException, IOException {

        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try {
            MARSHALLER.marshal(jaxbElement, baos);
            return baos.toByteArray();
        } finally {
            baos.close();
        }
    }
}

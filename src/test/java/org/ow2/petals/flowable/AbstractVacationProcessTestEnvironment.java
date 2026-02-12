/**
 * Copyright (c) 2014-2026 Linagora
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

import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.File;
import java.net.URISyntaxException;
import java.net.URL;

import javax.xml.namespace.QName;

import org.junit.jupiter.api.BeforeAll;
import org.ow2.petals.component.framework.junit.impl.ConsumesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ProvidesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration;
import org.ow2.petals.component.framework.junit.rule.ParameterGenerator;
import org.ow2.petals.component.framework.junit.rule.ServiceConfigurationFactory;

import com.ebmwebsourcing.easycommons.lang.UncheckedException;

import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Marshaller;

/**
 * Abstract class for unit tests about request processing
 * 
 * @author Christophe DENEUX - Linagora
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

    @BeforeAll
    private static void initJaxBTooling() {
        try {
            final JAXBContext context = JAXBContext.newInstance(
                    org.ow2.petals.se_flowable.unit_test.vacation.vacationservice.ObjectFactory.class,
                    org.ow2.petals.se_flowable.unit_test.vacation.archivageservice.ObjectFactory.class,
                    org.ow2.petals.components.flowable.generic._1.ObjectFactory.class);
            UNMARSHALLER = context.createUnmarshaller();
            MARSHALLER = context.createMarshaller();
            MARSHALLER.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
        } catch (final JAXBException e) {
            throw new UncheckedException(e);
        }
    }

    @BeforeAll
    private static void completesFlowableClientIdmEngineConfiguration() throws Exception {
        FLOWABLE_CLIENT.setIdmEngineConfiguratorCfgFile(
                getIdmEngineConfiguratorCfgFile(VACATION_SU_HOME + "idm-engine-configurator.properties"));
    }

    private static File getIdmEngineConfiguratorCfgFile(final String resourceName) throws URISyntaxException {
        final URL idmEngineConfiguratorCfg = Thread.currentThread().getContextClassLoader()
                .getResource(VACATION_SU_HOME + "idm-engine-configurator.properties");
        assertNotNull(idmEngineConfiguratorCfg, "IDM engine configurator config file is missing !");
        return new File(idmEngineConfiguratorCfg.toURI());
    }

    @BeforeAll
    private static void componentUnderTestIdmEngineConfiguration() throws Exception {
        COMPONENT_UNDER_TEST.setParameter(
                new QName(FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.IDM_ENGINE_CONFIGURATOR_CFG_FILE),
                // Generate identity service configuration files
                new ParameterGenerator() {

                    @Override
                    public String generate() throws Exception {
                        return getIdmEngineConfiguratorCfgFile(VACATION_SU_HOME + "idm-engine-configurator.properties")
                                .getAbsolutePath();
                    }
                });
    }

    protected static ServiceConfigurationFactory createVacationServiceCfgFactory() {

        return createVacationServiceCfgFactory(1);
    }

    protected static ServiceConfigurationFactory createVacationServiceCfgFactory(final int bpmnVersion) {

        return new ServiceConfigurationFactory() {
            @Override
            public ServiceConfiguration create() {

                final URL wsdlUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(VACATION_SU_HOME + "vacationRequest.wsdl");
                assertNotNull(wsdlUrl, "WSDL not found");
                final ProvidesServiceConfiguration serviceConfiguration = new ProvidesServiceConfiguration(
                        VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT, wsdlUrl);

                final URL demanderCongesResponseXslUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(VACATION_SU_HOME + "demanderCongesResponse.xsl");
                assertNotNull(demanderCongesResponseXslUrl, "Output XSL 'demanderCongesResponse.xsl' not found");
                serviceConfiguration.addResource(demanderCongesResponseXslUrl);

                final URL validerDemandeResponseXslUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(VACATION_SU_HOME + "validerDemandeResponse.xsl");
                assertNotNull(validerDemandeResponseXslUrl, "Output XSL 'validerDemandeResponse.xsl' not found");
                serviceConfiguration.addResource(validerDemandeResponseXslUrl);

                final URL ajusterDemandeResponseXslUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(VACATION_SU_HOME + "ajusterDemandeResponse.xsl");
                assertNotNull(ajusterDemandeResponseXslUrl, "Output XSL 'ajusterDemandeResponse.xsl' not found");
                serviceConfiguration.addResource(ajusterDemandeResponseXslUrl);

                final URL numeroDemandeInconnuXslUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(VACATION_SU_HOME + "numeroDemandeInconnu.xsl");
                assertNotNull(numeroDemandeInconnuXslUrl, "Output XSL 'numeroDemandeInconnu.xsl' not found");
                serviceConfiguration.addResource(numeroDemandeInconnuXslUrl);

                final URL demandeDejaValideeXslUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(VACATION_SU_HOME + "demandeDejaValidee.xsl");
                assertNotNull(demandeDejaValideeXslUrl, "Output XSL 'demandeDejaValidee.xsl' not found");
                serviceConfiguration.addResource(demandeDejaValideeXslUrl);

                final URL bpmnUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(VACATION_SU_HOME + "vacationRequest.bpmn20.xml");
                assertNotNull(bpmnUrl, "BPMN file not found");
                serviceConfiguration.addResource(bpmnUrl);

                final URL archivageServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                        .getResource(VACATION_SU_HOME + "archivageService.wsdl");
                assertNotNull(archivageServiceWsdlUrl, "archivageService WSDL not found");
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
}
